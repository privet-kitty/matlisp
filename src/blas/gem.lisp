(in-package #:matlisp)

;;
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

(deft/generic (t/blas-gemm! #'subtypep) sym (alpha A lda B ldb beta C ldc transa opa opb))
(deft/method (t/blas-gemm! #'blas-tensor-typep) (sym dense-tensor) (alpha A lda B ldb beta C ldc transa opa opb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (alpha A lda B ldb beta C ldc transa opa opb) (m n k))   
      `(let* (,@decl
	      (,m (dimensions ,C 0)) (,n (dimensions ,C 1))
	      (,k (dimensions ,A (ecase ,transa (#\N 1) ((#\T #\C) 0)))))
	 (declare (type ,sym ,A ,B ,C)
		  (type ,(field-type sym) ,alpha ,beta)
		  (type index-type ,lda ,ldb ,ldc ,m ,n ,k)
		  (type character ,transa ,opa ,opb))
	 (ffuncall ,(blas-func "gemm" ftype)
		   (:& :character) ,opa (:& :character) ,opb
		   (:& :integer) ,m (:& :integer) ,n (:& :integer) ,k
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
		   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ldb
		   (:& ,(lisp->ffc ftype t)) ,beta
		   (:* ,(lisp->ffc ftype) :+ (head ,C)) (the ,(store-type sym) (store ,C)) (:& :integer) ,ldc)
	 ,C))))
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

(deft/generic (t/gemm! #'subtypep) sym (alpha A B beta C transa transb))
(deft/method t/gemm! (sym dense-tensor) (alpha A B beta C transa transb)
  (using-gensyms (decl (alpha A B beta C transa transb))
   `(let (,@decl)
      (declare (type ,sym ,A ,B ,C)
	       (type ,(field-type sym) ,alpha ,beta)
	       (type character ,transa ,transb))
      (unless (t/f= ,(field-type sym) ,beta (t/fid* ,(field-type sym)))
	(t/scdi! ,sym ,beta ,C :scal? t :numx? t))
      ,@(when (field-realp (field-type sym))
	      `((when (char= ,transa #\C) (setq ,transa #\T))
		(when (char= ,transb #\C) (setq ,transb #\T))))
      ;;These loops are optimized for column major matrices
      ,(labels ((transpose-ref (mat)
		  `(ref ,(cadr mat) ,@(reverse (cddr mat))))
		(conjugate-ref (mat)
		  `(t/fc ,(field-type sym) ,mat))
		(generate-mm-code (transa transb)
		  (destructuring-bind (A-ref B-ref) (mapcar #'(lambda (mat trans) (ecase trans
										    ((#\N #\T) mat)
										    ((#\C) (conjugate-ref mat))))
							    (mapcar #'(lambda (mat trans) (ecase trans
											    ((#\N) mat)
											    ((#\T #\C) (transpose-ref mat))))
								    (list `(ref ,A i k) `(ref ,B k j)) (list transa transb))
							    (list transa transb))
		    (let ((loopo (let ((ta (member transa '(#\T #\C)))
				       (tb (member transb '(#\T #\C))))
				      (cond
					((and (not ta) (not tb)) `(j k i))
					((and (not ta) tb) `(k j i))
					(t`(i j k))))))
		      `(einstein-sum ,sym ,loopo (ref ,C i j) (* ,alpha ,A-ref ,B-ref) nil)))))
	       `(ecase ,transa
		  ,@(loop :for ta :across (if (field-realp (field-type sym)) "NT" "NTC")
		       :collect `(,ta (ecase ,transb
					,@(loop :for tb :across (if (field-realp (field-type sym)) "NT" "NTC")
					     :collect `(,tb ,(generate-mm-code ta tb))))))))
      ,C)))
;;---------------------------------------------------------------;;
(defgeneric gem! (alpha A B beta C &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEM! alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix/Vector Multiplication given by
	       --      -      -

	    C <- alpha * op(A) * op(B) + beta * C

  and returns C.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

  JOB must be a keyword with two of these alphabets
     N                 Identity
     T                 Transpose
     C                 Hermitian transpose {conjugate transpose}
")
  (:method :before (alpha (A dense-tensor) (B dense-tensor) beta (C dense-tensor) &optional (job :nn))
    (assert (not (or (eq A C) (eq B C))) nil 'invalid-arguments :message "GEM!: C = {A or B} is not allowed.")
    (letv* (((joba &optional (jobb #\N)) (split-job job)))
      (assert (and
	       (tensor-matrixp A)
	       (= (order B) (order C)) (if (char= jobb #\N) (<= (order C) 2) (tensor-matrixp C))
	       (let ((loga (ecase joba (#\N 0) ((#\T #\C) 1)))
		     (logb (ecase jobb (#\N 0) ((#\T #\C) 1))))
		 (and (= (dimensions C 0) (dimensions A (logxor 0 loga)))
		      (= (dimensions A (logxor 1 loga)) (dimensions B (logxor 0 logb)))
		      (or (not (tensor-matrixp C)) (= (dimensions C 1) (dimensions B (logxor 1 logb)))))))
	      nil 'tensor-dimension-mismatch))))

(define-tensor-method gem! (alpha (A dense-tensor :x) (B dense-tensor :x) beta (C dense-tensor :x t) &optional (job :n))
  `(letv* ((alpha (t/coerce ,(field-type (cl a)) alpha))
	   (beta (t/coerce ,(field-type (cl a)) beta))
	   ((joba &optional (jobb #\N)) (split-job job)))
     (declare (type ,(field-type (cl a)) alpha beta)
	      (type base-char joba jobb))
     (if (tensor-vectorp C)
	 ,(recursive-append
	   (when (blas-tensor-typep (cl B))
	     `(if (call-fortran? A (t/blas-lb ,(cl a) 2))
		  (with-columnification (((A joba)) ())
		    (letv* ((lda opa (blas-matrix-compatiblep A joba)))
		      (t/blas-gemv! ,(cl a) alpha A lda B (strides B 0) beta C (strides C 0) opa)))))
	   `(t/gemv! ,(cl a) alpha A B beta C joba))
	 ,(recursive-append
	   (when (blas-tensor-typep (cl c))
	     `(if (call-fortran? C (t/blas-lb ,(cl c) 3))
		  (with-columnification (((a joba) (b jobb)) (c))
		    (letv* ((lda opa (blas-matrix-compatiblep a joba))
			    (ldb opb (blas-matrix-compatiblep b jobb)))
		      (t/blas-gemm! ,(cl a) alpha A lda B ldb beta C (or (blas-matrix-compatiblep c #\N) 0) joba opa opb)))))
	   `(t/gemm! ,(cl a) alpha A B beta C joba jobb))))
  'C)

;;---------------------------------------------------------------;;
(defgeneric gem (alpha a b beta c &optional job)
  (:documentation
   "
  Syntax
  ======
  (GEM alpha a b beta c [job])

  Purpose
  =======
  Performs the GEneral Matrix Multiplication given by
	       --      -      -

	     alpha * op(A) * op(B) + beta * C

  and returns the result in a new matrix.

  alpha,beta are scalars and A,B,C are matrices.
  op(A) means either A or A'.

  JOB must be a keyword with two of these alphabets
     N                 Identity
     T                 Transpose
     C                 Hermitian conjugate
"))

(defmethod gem (alpha (A dense-tensor) (B dense-tensor) beta (C dense-tensor) &optional (job :nn))
  (gem! alpha A B beta (copy C) job))

(defmethod gem (alpha (A dense-tensor) (B dense-tensor) (beta (eql nil)) (C (eql nil)) &optional (job :nn))
  (gem! alpha A B 0
	(letv* (((joba &optional (jobb #\N)) (split-job job)))	  
	  (zeros (list* (dimensions A (ecase joba (#\N 0) ((#\C #\T) 1)))
			(when (tensor-matrixp B) (list (dimensions B (ecase jobb (#\N 1) ((#\C #\T) 0))))))
		 (ziprm (cclass-max class-of) (A B))))
	job))
