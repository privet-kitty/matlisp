(in-package #:matlisp-ffi)

(closer-mop:defclass foreign-vector-class (standard-class)
  ((ffi-type :reader ffi-type)))
(closer-mop:defmethod closer-mop:validate-superclass ((class foreign-vector-class) (superclass standard-class))  t)
(defmethod ffi-type ((cl symbol)) (ffi-type (find-class cl)))
;;
(closer-mop:defclass foreign-vector ()
  ((ptr :initarg :ptr :initform (cffi:null-pointer))
   (length :initarg :length :initform 0))
  (:metaclass foreign-vector-class))

(with-memoization ()
  (memoizing
   (defun foreign-vector (ffi-type)
     (or (if-let (class (find ffi-type (closer-mop:class-direct-subclasses (find-class 'foreign-vector)) :key #'ffi-type))
	   (class-name class))
	 (let* ((cl-name (intern (format nil "<FOREIGN-VECTOR: ~a>"  ffi-type) (find-package "MATLISP-FFI"))))
	   (assert (member (ffc->cffi ffi-type) '(:double :float :int32 :int64)) nil)
	   (compile-and-eval
	    `(progn
	       (closer-mop:defclass ,cl-name (foreign-vector) ()
		 (:metaclass foreign-vector-class))
	       (setf (slot-value (find-class ',cl-name) 'ffi-type) ,ffi-type)))
	   cl-name)))))
;;
(defparameter *fvref-range-check* t)

(defun fvref (x i)
  (declare (type foreign-vector x))
  (let ((n (slot-value (the foreign-vector x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)
    (cffi:mem-aref (slot-value x 'ptr) (ffc->cffi (ffi-type (class-of x))) i)))

(define-compiler-macro fvref (&whole form x i)
  (match x
    ((list 'the (and (type symbol) (guard fv (subtypep fv 'foreign-vector))) obj)
     (let ((cffi-type (ffc->cffi (ffi-type fv))))
       (with-gensyms (obj-v i-v n-v)
	 `(let-typed ((,obj-v ,obj :type ,fv)
		      (,i-v ,i :type fixnum))
	    ,@(if *fvref-range-check*
		  `((let ((,n-v (slot-value ,obj-v 'length)))
		      (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
	    (cffi:mem-ref (slot-value (the ,fv ,obj-v) 'ptr) ,cffi-type (the fixnum (* (the fixnum ,i-v) (the fixnum ,(cffi:foreign-type-size cffi-type)))))))))
    (_ form)))

(defun (setf fvref) (value x i)
  (declare (type foreign-vector x))
  (let ((n (slot-value (the foreign-vector x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)
    (setf (cffi:mem-aref (slot-value x 'ptr) (ffc->cffi (ffi-type (class-of x))) i) (coerce value (%ffc->lisp (ffi-type (class-of x)))))))

(define-compiler-macro (setf fvref) (&whole form value x i)
  (multiple-value-match (values x value)
    (((list 'the (and (type symbol) (guard fv (subtypep fv 'foreign-vector))) obj)
      (list 'the (and (type symbol) (guard lt (eql lt (%ffc->lisp (ffi-type fv))))) val))
     (let ((cffi-type (ffc->cffi (ffi-type fv))))
       (with-gensyms (obj-v i-v n-v)
	 `(let-typed ((,obj-v ,obj :type ,fv)
		      (,i-v ,i :type fixnum))
	    ,@(if *fvref-range-check*
		  `((let ((,n-v (slot-value ,obj-v 'length)))
		      (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
	    (setf (cffi:mem-ref (slot-value (the ,fv ,obj-v) 'ptr) ,cffi-type (the fixnum (* (the fixnum ,i-v) (the fixnum ,(cffi:foreign-type-size cffi-type)))))
		  (the ,lt ,val))))))
    (_ form)))
;;
