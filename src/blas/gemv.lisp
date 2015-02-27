(in-package #:matlisp)

(deft/generic (t/blas-gemv! #'subtypep) sym (alpha A lda x st-x beta y st-y transp))
(deft/method (t/blas-gemv! #'blas-tensor-typep) (sym dense-tensor) (alpha A lda x st-x beta y st-y transp)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha A lda x st-x beta y st-y transp) (m n))
      `(let* (,@decl
	      (,m (dimensions ,A 0))
	      (,n (dimensions ,A 1)))
	 (declare (type ,sym ,A ,x ,y)
		  (type ,(field-type sym) ,alpha ,beta)
		  (type index-type ,st-x ,st-y ,lda ,m ,n))
	 (ffuncall ,(blas-func "gemv" ftype)
		   (:& :character) ,transp
		   (:& :integer) ,m (:& :integer) ,n
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
		   (:* ,(lisp->ffc ftype) :+ (head ,x)) (the ,(store-type sym) (store ,x)) (:& :integer) ,st-x
		   (:& ,(lisp->ffc ftype t)) ,beta
		   (:* ,(lisp->ffc ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :integer) ,st-y)
	 ,y))))

;;
(deft/generic (t/gemv! #'subtypep) sym (alpha A x beta y transp))

(deft/method t/gemv! (sym dense-tensor) (alpha A x beta y transp)
  (using-gensyms (decl (alpha A x beta y transp))
   `(let (,@decl)
      (declare (type ,sym ,A ,x ,y)
	       (type ,(field-type sym) ,alpha ,beta)
	       (type character ,transp))
      (scal! ,beta ,y)
      ,@(when (field-realp (field-type sym))
	      `((when (char= ,transp #\C) (setq ,transp #\T))))
      ;;These loops are optimized for column major matrices
      (ecase ,transp
       (#\N (einstein-sum ,sym (j i) (ref ,y i) (* ,alpha (ref ,A i j) (ref ,x j)) nil))
       (#\T (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (ref ,A j i) (ref ,x j)) nil))
       ,@(unless (field-realp (field-type sym))
		 `((#\C (einstein-sum ,sym (i j) (ref ,y i) (* ,alpha (t/fc ,(field-type sym) (ref ,A j i)) (ref ,x j)) nil)))))
      ,y)))
;;---------------------------------------------------------------;;

(defgeneric gemv! (alpha A x beta y &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMV! alpha A x beta y [job])

  Purpose
  =======
  Performs the GEneral Matrix Vector operation given by
	       --      -      -

	    Y <- alpha * op(A) * x + beta * y

  and returns y.

  alpha,beta are scalars,
  A is a matrix, and x,y are vectors.

  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :N (default)      alpha * A * x + beta * y
     :T                alpha * A^T * x + beta * y
     :C                alpha * A^H * x + beta * y
")
  (:method :before (alpha (A dense-tensor) (x dense-tensor) beta (y dense-tensor) &optional (job :n))
    (assert (and (tensor-vectorp x) (tensor-vectorp y) (tensor-matrixp A)
		 (= (dimensions x 0) (dimensions A (ecase job (:n 1) ((:t :c) 0))))
		 (= (dimensions y 0) (dimensions A (ecase job (:n 0) ((:t :c) 1)))))
	    nil 'tensor-dimension-mismatch)
    (assert (not (eq x y)) nil 'invalid-arguments :message "GEMV!: x and y cannot be the same vector")))

(define-tensor-method gemv! (alpha (A dense-tensor :x) (x dense-tensor :x) beta (y dense-tensor :x t) &optional (job :n))
  `(let ((alpha (t/coerce ,(field-type (cl x)) alpha))
	 (beta (t/coerce ,(field-type (cl x)) beta))
	 (cjob (aref (symbol-name job) 0)))
     (declare (type ,(field-type (cl x)) alpha beta)
	      (type character cjob))
     ,(recursive-append
       (when (blas-tensor-typep (cl x))
	 `(if (call-fortran? A (t/blas-lb ,(cl a) 2))
	      (with-columnification (((A cjob)) ())
		(multiple-value-bind (lda opa) (blas-matrix-compatiblep A cjob)
		  (t/blas-gemv! ,(cl a) alpha A lda
				x (strides x 0)
				beta
				y (strides y 0)
				opa)))))
       `(t/gemv! ,(cl a) alpha A x beta y cjob)))
  'y)
;;---------------------------------------------------------------;;
(defgeneric gemv (alpha A x beta y &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEMV alpha A x beta y [job])

  Purpose
  =======
  Returns the GEneral Matrix Vector operation given by

	    alpha * op(A) * x + beta * y

  alpha,beta are scalars,
  A is a matrix, and x,y are vectors.

  op(A) means either A or A'.

     JOB                    Operation
  ---------------------------------------------------
     :N (default)      alpha * A * x + beta * y
     :T                alpha * A^T * x + beta * y
     :C                alpha * A^H * x + beta * y
"))

(defmethod gemv (alpha (A dense-tensor) (x dense-tensor) beta (y dense-tensor) &optional (job :n))
  (gemv! alpha A x beta (copy y) job))

(defmethod gemv (alpha (A dense-tensor) (x dense-tensor) (beta (eql nil)) (y (eql nil)) &optional (job :n))
  (gemv! alpha A x 1 (zeros (dimensions A (ecase job (:n 0) ((:t :c) 1))) (ziprm (cclass-max class-of) A)) job))
