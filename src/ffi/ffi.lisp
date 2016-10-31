(in-package #:matlisp-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;

(defun lisp->mffi (type)
  "Convert the given matlisp-ffi type into one understood by Lisp"
  (ematch type
    ('character :char)
    ('string :string)
    ;;int types
    ((list 'signed-byte 8) :char)
    ((list 'unsigned-byte 8) :unsigned-char)
    ((list 'signed-byte 16) :short)
    ((list 'usigned-byte 16) :unsigned-short)
    ((list 'signed-byte 32) :int)
    ((list 'unsigned-byte 32) :unsigned-int)
    ((list 'signed-byte 64) :long)
    ((list 'unsigned-byte 64) :unsigned-long)
    ('single-float :float)
    ('double-float :double)
    ;;
    ((list 'cl:complex element-type)
     (let ((ffi-type (lisp->mffi element-type)))
       (values `(:complex ,ffi-type) `(:pointer (,ffi-type 2)))))
    ((list 'simple-array element-type '(*))
     (letv* ((element-ffi-type element-cffi-type (lisp->mffi element-type)))
       (values `(:* ,element-ffi-type) (match element-ffi-type
					 ((list :complex _) element-cffi-type)
					 (_ `(:pointer ,element-ffi-type))))))))

#+nil
(mffi->lisp '(:& (:complex :double)))

(defun mffi->lisp (type)
  "Convert the given matlisp-ffi type into one understood by CFFI."
  (ematch type
    (:char 'character)
    (:string 'string)
    (:char (list 'signed-byte 8))
    (:unsigned-char (list 'unsigned-byte 8))
    (:short (list 'signed-byte 16))
    (:unsigned-short (list 'usigned-byte 16))
    (:int (list 'signed-byte 32))
    (:unsigned-int (list 'unsigned-byte 32))
    (:long (list 'signed-byte 64))
    (:unsigned-long (list 'unsigned-byte 64))
    (:float 'single-float)
    (:double 'double-float)
    ;;Incompatibilities
    (:* (values 'cffi:foreign-pointer `(:pointer :void)))
    (:callback (values 'symbol `(:pointer :void)))
    ((lambda-list :& ref-type &rest _)
     (ematch ref-type
       ((list :complex element-type) (values `(complex ,(mffi->lisp element-type)) `(:pointer (,element-type 2))))
       ((or :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :float :double) (values (mffi->lisp ref-type) `(:pointer ,ref-type)))))
    ((lambda-list :* ptr-type &rest _)
     (ematch ptr-type
       ((list :complex element-type) (values `(simple-array ,(mffi->lisp element-type) (*)) `(:pointer (,element-type 2))))
       ((or (and pointer-type (or :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :float :double))
	    (list (and pointer-type (or :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :float :double)) (guard n (< 0 n))))
	(values `(simple-array ,(mffi->lisp pointer-type) (*)) `(:pointer ,@(if n `((,pointer-type ,n)) `(,pointer-type)))))))))
;;
(defun parse-ffargs (args &optional append-string-length?)
  (labels ((argument-decl (type expr)
	     (ematch type
	       (:callback (list :argument `(cffi-sys:%callback (the ,(mffi->lisp type) ,expr))))
	       (:string (if append-string-length?
			    (with-gensyms (s)
			      (list  :argument s :let-bind `(,s ,expr :type ,(mffi->lisp type))
				     :aux `(:unsigned-int (the (unsigned-byte 32) (length ,s)))))
			    (list :argument `(the ,(mffi->lisp type) ,expr))))
	       ((type symbol) (list :argument `(the ,(mffi->lisp type) ,expr)))
	       ((lambda-list :& sub-type &optional ((and output (or nil :output))) &aux (utype (second (nth-value 1 (mffi->lisp type)))) (var (gensym "var")) (c (gensym "expr")))
		(list* :argument `(the cffi:foreign-pointer ,var)
		       (ematch sub-type
			 ((list :complex (type symbol))
			  (ematch utype
			    ((list utype 2)
			     (list :alloc `(,var ,utype :count 2)
				   :init `(let-typed ((,c ,expr :type ,(mffi->lisp type)))
					    (setf (cffi:mem-aref ,var ,utype 0) (cl:realpart ,c)
						  (cffi:mem-aref ,var ,utype 1) (cl:imagpart ,c)))
				   :output (when output `(complex (cffi:mem-aref ,var ,utype 0) (cffi:mem-aref ,var ,utype 1)))))))
			 ((or :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :float :double)
			  (list :alloc `(,var ,utype :initial-element ,(recursive-append
									(when (eq sub-type :char) `(char-code))
									`(the ,(mffi->lisp type) ,expr)))
				:output (when output `(cffi:mem-ref ,var ,utype)))))))
	       ((lambda-list :* _ &key + &aux (vec (gensym "vec")))
		(letv* ((lisp-type ffi-type (mffi->lisp type)))
		  (list :argument (let ((ptr `(etypecase ,vec
						(,lisp-type (vector-sap-interpreter-specific ,vec))
						(cffi:foreign-pointer ,vec)
						(,(foreign-vector (ematch lisp-type ((list 'simple-array element-type '(*)) element-type))) (slot-value ,vec 'ptr)))))
				    (if +
					`(cffi:inc-pointer ,ptr (* (the fixnum ,+)
								   ,(ematch ffi-type
								      ((list :pointer (and element-type (type symbol))) (cffi:foreign-type-size element-type))
								      ((list :pointer (list (and element-type (type symbol)) (guard n (< 0 n))))
								       (* n (cffi:foreign-type-size element-type))))))
					ptr))
			:let-bind `(,vec ,expr ,@(match expr ((list 'the type _) `(:type ,type))))))))))
    (loop :for (type expr) :on args :by #'cddr
       :collect (list* :cffi (match type
			       ((or :string :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :float :double) type)
			       (_ :pointer))
		       (argument-decl type expr)))))
;;
(defparameter *f77-name-mangler*
  (find-if #'(lambda (f) (cffi:foreign-symbol-pointer (funcall f "ddot")))
	   (mapcart #'(lambda (a b) (compile-and-eval `(lambda (x) ,(subst b 'x a))))
		    '((string-upcase x) (string-downcase x)) '((identity x) (string+ x "_") (string+ x "__")))))

(defmacro ffuncall (name-&-return-type &rest args)
  "This macro provides a thin wrapper around cffi:foreign-funcall for making calls to Fortran functions
that much easier. We use the F2C convention, which isn't really followed by compilers when returning
complex values (so be wary of complex number returning functions).

 (FFUNCALL (name &optional (return-type :void) (mode :f2c)) *[type arg])

If (eq mode :f2c) then a name mangler is called on name, and string lengths are appended at the
end of the argument to the foreign function. Neither of this is done if (eq mode :c).

Type (credits for aesthetics goes to CCL) is of the general form:
 type -> Pass by value.
 (:& type &key output) -> Pass by reference, if 'output' return value after exiting from foreign function.
 (:* type &key +) -> Pointer/Array/Foreign-vector, if '+' increment pointer by '+' times foreign-type-size.
There are restrictions as to what types can be used with '(:& :*), see source of lisp->mffi and mffi->lisp.

Example:
@lisp
> (let ((a (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0))))
    (ffuncall (\"zdotc\" :double-float) (:& :integer) (/ (length a) 2)
	      (:* :complex-double-float) a (:& :integer) 1
	      (:* :complex-double-float) a (:& :integer) 1))
=> 30d0
> (let ((a (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0))))
    (ffuncall (\"ddot\" :double-float) (:& :integer) (length a)
	      (:* :double-float) a (:& :integer) 1
	      (:* :double-float) a (:& :integer) 1))
=> 30d0
@end lisp
"
  (destructuring-bind (name &optional (return-type :void) (mode :f2c)) (ensure-list name-&-return-type)
    (match return-type
      ((list :complex type) `(ffuncall (,name :void ,mode) (:& ,return-type :output) ,(coerce #c(0 0) (mffi->lisp `(:& ,return-type))) ,@args))
      (_ (let ((pargs (parse-ffargs args (when (eq mode :f2c) t))))
	   (labels ((mapf (place) (remove-if #'null (mapcar #'(lambda (x) (getf x place)) pargs))))
	     `(with-fortran-float-modes
		(without-gcing
		  ,(recursive-append
		    (when-let ((bd (mapf :let-bind)))
		      `(let-typed (,@bd)))
		    (when-let ((al (mapf :alloc)))
		      `(with-foreign-objects-stacked (,@al)))
		    (when-let ((init (mapf :init)))
		      `(progn ,@init))
		    (let ((callc `(cffi-sys:%foreign-funcall ,(ecase mode
								     (:f2c (funcall *f77-name-mangler* name))
								     (:c name))
							     (,@(apply #'append (zip (mapf :cffi) (mapf :argument))) ,@(apply #'append (mapf :aux)) ,(if (eq return-type :void) :void (first (ensure-list return-type)))))))
		      (if (eq return-type :void)
			  `(progn ,callc (values ,@(mapf :output)))
			  (with-gensyms (ret)
			    `(let ((,ret ,callc))
			       (values ,ret ,@(mapf :output)))))))))))))))

(defun vector-sap (v &optional (inc 0))
  (let* ((ptr (matlisp-ffi::vector-sap-interpreter-specific v)))
    (if (zerop inc)
	ptr
	(cffi:inc-pointer ptr (* inc (cffi:foreign-type-size
				      (matlisp-ffi:lisp->mffi (array-element-type v))))))))

)
