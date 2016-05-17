(in-package #:matlisp-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct (wrap (:constructor make-wrap (ptr)))
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))
;;(simple-array (or single-float double-float (signed-byte 32) (signed-byte 64)) (*))
;;

(labels ((ffc->rest (type)
	   (ematch type
	     (:double-float (values :double 'double-float))
	     (:single-float (values :float 'single-float))
	     (:character (values :char 'character))
	     (:string (values :string 'string))
	     (:integer (values :int '(signed-byte 32))) ;;int32
	     (:long (values :long '(signed-byte 64)))	;;int64
	     (:* (values `(:pointer :void) 'cffi:foreign-pointer))
	     (:callback (values `(:pointer :void) 'symbol))
	     ((λlist :& ref-type &rest _)
	      (ecase ref-type
		(:complex-single-float
		 (values `(:pointer ,(ffc->rest :single-float)) `(complex single-float)))
		(:complex-double-float
		 (values `(:pointer ,(ffc->rest :double-float)) `(complex double-float)))
		((:double-float :single-float :character :integer :long)
		 (letv* ((cffi lisp (ffc->rest ref-type)))
		   (values `(:pointer ,cffi) lisp)))))
	     ((λlist :* ptr-type &rest _)
	      (ematch ptr-type
		(:complex-single-float (values `(:pointer ,(ffc->rest :single-float)) `(simple-array single-float (*))))
		(:complex-double-float (values `(:pointer ,(ffc->cffi :double-float)) `(simple-array double-float (*))))
		((or (and pointer-type (or :double-float :single-float :integer :long :character))
		     (list (and pointer-type (or :double-float :single-float :integer :long :character)) (guard n (< 0 n))))
		 (values `(:pointer ,(ffc->cffi pointer-type)) `(simple-array ,(ffc->lisp pointer-type) (*)))))))))
  (defun ffc->lisp (type)
    "Convert the given matlisp-ffi type into one understood by CFFI."
    (nth-value 1 (ffc->rest type)))
  (defun ffc->cffi (type)
    "Convert the given matlisp-ffi type into one understood by Lisp"
    (nth-value 0 (ffc->rest type))))

(defun lisp->ffc (type &optional refp)
  "Convert the given matlisp-ffi type into one understood by Lisp"
  (ematch type
    ('cl:character :character)
    ('cl:string :string)
    ((list 'cl:signed-byte 32) :integer)
    ((list 'cl:signed-byte 64) :long)
    ('cl:single-float :single-float)
    ('cl:double-float :double-float)
    ((list 'cl:complex type)
     (if refp
	 (ecase type (:single-float :complex-single-float) (:double-float :complex-double-float))
	 (list (lisp->ffc type) 2)))))

;; type -> Pass by value
;; (:& type &key output) -> Pass by reference, if 'output' return value after exiting from foreign function.
;; (:* type &key +) -> Pointer/Array/Foreign-vector, if '+' increment pointer by '+' times foreign-type-size.
(defun %ffc.parse-ffargs (args &optional append-string-length?)
  (labels ((argument-decl (type expr)
	     (ematch type
	       (:callback (list :argument `(cffi-sys:%callback (the ,(ffc->lisp type) ,expr))))
	       (:string (if append-string-length?
			    (with-gensyms (s)
			      (list  :argument s :let-bind `(,s ,expr :type ,(ffc->lisp type))
				     :aux `(:int (the fixnum (length ,s)))))
			    (list :argument `(the ,(ffc->lisp type) ,expr))))
	       ((type symbol) (list :argument `(the ,(ffc->lisp type) ,expr)))
	       ((λlist :& sub-type &optional ((and output (or (null) :output))) &aux (utype (second (ffc->cffi type))) (var (gensym "var")) (c (gensym "expr")))
		(list* :argument `(the cffi:foreign-pointer ,var)
		       (ematch sub-type
			 ((or :complex-double-float :complex-single-float)
			  (list :alloc `(,var ,utype :count 2)
				:init `(let-typed ((,c ,expr :type ,(ffc->lisp type)))
					 (setf (cffi:mem-aref ,var ,utype 0) (realpart ,c)
					       (cffi:mem-aref ,var ,utype 1) (imagpart ,c)))
				:output (when output `(complex (cffi:mem-aref ,var ,utype 0) (cffi:mem-aref ,var ,utype 1)))))
			 ((or :double-float :single-float :character :integer :long)
			  (list :alloc `(,var ,utype :initial-element ,(recursive-append
									(when (eq sub-type :character) `(char-code))
									`(the ,(ffc->lisp type) ,expr)))
				:output (when output `(cffi:mem-ref ,var ,utype)))))))
	       ((λlist :* sub-type &key + &aux (vec (gensym "vec")))
		(list :argument (let ((ptr `(etypecase ,vec
					      (,(ffc->lisp type) (vector-sap-interpreter-specific ,vec))
					      (cffi:foreign-pointer ,vec)
					      (foreign-vector (slot-value ,vec 'ptr)))))
				  (if +
				      `(cffi:inc-pointer ,ptr (* (the fixnum ,+)
								 ,(ematch sub-type
								    (:complex-single-float (* 2 (cffi:foreign-type-size (ffc->cffi :single-float))))
								    (:complex-double-float (* 2 (cffi:foreign-type-size (ffc->cffi :double-float))))
								    ((or :double-float :single-float :integer :long) (cffi:foreign-type-size (ffc->cffi sub-type)))
								    ((list (and pointer-type (or :double-float :single-float :integer :long :character)) (guard n (< 0 n)))
								     (* n (cffi:foreign-type-size (ffc->cffi pointer-type)))))))
				      ptr))
		      :let-bind `(,vec ,expr ,@(when (and (consp expr) (eq (first expr) 'cl:the)) `(:type ,(second expr)))))))))
    (loop :for (type expr) :on args :by #'cddr
       :collect (list* :cffi (let ((ctype (ffc->cffi type))) (if (consp ctype) (first ctype) ctype)) (argument-decl type expr)))))

;;
(define-constant +f77-name-mangler+
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
There are restrictions as to what types can be used with '(:& :*), see source of ffc->cffi and ffc->lisp.

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
    (if (member return-type '(:complex-single-float :complex-double-float))
	`(ffuncall (,name :void ,mode) (:& ,return-type :output) ,(coerce #c(0 0) (ffc->lisp `(:& ,return-type))) ,@args)
	(let ((pargs (%ffc.parse-ffargs args (when (eq mode :f2c) t))))
	  (labels ((mapf (place) (remove-if #'null (mapcar #'(lambda (x) (getf x place)) pargs))))
	    `(with-fortran-float-modes
	       (without-gcing
		 ,(recursive-append
		   (when-let (bd (mapf :let-bind))
		     `(let-typed (,@bd)))
		   (when-let (al (mapf :alloc))
		     `(with-foreign-objects-stacked (,@al)))
		   (when-let (init (mapf :init))
		     `(progn ,@init))
		   (let ((callc `(cffi-sys:%foreign-funcall ,(ecase mode
								    (:f2c (funcall +f77-name-mangler+ name))
								    (:c name))
							    (,@(apply #'append (zip (mapf :cffi) (mapf :argument))) ,@(apply #'append (mapf :aux)) ,(if (eq return-type :void) :void (first (ensure-list (ffc->cffi return-type))))))))
		     (if (eq return-type :void)
			 `(progn ,callc (values ,@(mapf :output)))
			 (with-gensyms (ret)
			   `(let ((,ret ,callc))
			      (values ,ret ,@(mapf :output))))))))))))))
)
