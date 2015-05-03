(in-package #:matlisp)

;;
(deft/generic (t/blas-trsm! #'subtypep) sym (side uplo transA diagA alpha A lda B ldb))
(deft/method (t/blas-trsm! #'blas-tensor-typep) (sym dense-tensor) (side uplo transA diagA alpha A lda B ldb)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (side uplo transA diagA alpha A lda B ldb))
      `(let* (,@decl)
	 (declare (type ,sym ,A ,B)
		  (type ,(field-type sym) ,alpha)
		  (type index-type ,lda ,ldb)
		  (type character ,transa ,diaga ,uplo ,side))
	 (ffuncall ,(blas-func "trsm" ftype)
		   (:& :character) ,side (:& :character) ,uplo (:& :character) ,transa (:& :character) ,diagA
		   (:& :integer) (dimensions ,B 0) (:& :integer) (dimensions ,B 1)
		   (:& ,(lisp->ffc ftype t)) ,alpha
		   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
		   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,ldb)
	 ,B))))

(deft/generic (t/blas-trsv! #'subtypep) sym (uplo transA diagA A lda b st-b))
(deft/method (t/blas-trsv! #'blas-tensor-typep) (sym dense-tensor) (uplo transA diagA A lda b st-b)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (uplo transA diagA A lda b st-b))
      `(let* (,@decl)
	 (declare (type ,sym ,A ,b)
		  (type index-type ,lda ,st-b)
		  (type character ,transa ,diaga ,uplo))
	 (ffuncall ,(blas-func "trsv" ftype)
	   (:& :character) ,uplo (:& :character) ,transa (:& :character) ,diagA
	   (:& :integer) (dimensions ,A 0)
	   (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	   (:* ,(lisp->ffc ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :integer) ,st-b)
	 ,b))))
;;
(defmacro inline-member (x lst &optional (test 'eql))
  (with-gensyms (xx) `(let ((,xx ,x)) (or ,@(mapcar #'(lambda (l) `(,test ,xx ,l)) lst)))))

(defgeneric trs! (alpha A b &optional solve uplo)
  (:method :before (alpha (A dense-tensor) (b dense-tensor) &optional (solve :nn) (uplo *default-uplo*))
     (destructuring-bind (joba diag &optional (side #\L sidep)) (split-job solve)
       (assert (and (inline-member joba (#\N #\T #\C) char=) (inline-member side (#\L #\R) char=) (inline-member diag (#\U #\N) char=)
		    (inline-member uplo (:u :l))) nil 'invalid-arguments)
       (assert (and (typep A 'tensor-square-matrix) (= (order b) (if sidep 2 1))
		    (= (dimensions A 0) (dimensions B (ecase side (#\L 0) (#\R 1))))) nil 'tensor-dimension-mismatch))))

(define-tensor-method trs! (alpha (A dense-tensor :x) (b dense-tensor :x) &optional  (solve :nn) (uplo *default-uplo*))
  `(destructuring-bind (joba diag &optional (side #\L)) (split-job solve)
     (with-columnification (((A joba)) ())
       (letv* ((lda opa (blas-matrix-compatiblep a joba))
	       (uplo (let ((c (aref (symbol-name uplo) 0)))
		       (if (char= opa joba) c (ecase c (#\U #\L) (#\L #\U)))))
	       (alpha (t/coerce ,(field-type (cl a)) alpha) :type ,(field-type (cl a))))
	 (declare (type base-char opa upa))
	 (if (tensor-vectorp b)
	     (t/blas-trsv! ,(cl a) uplo opa diag a lda (scal! alpha b) (strides b 0))
	     (with-columnification (() (b))
	       (t/blas-trsm! ,(cl a) side uplo opa diag alpha a lda b (blas-matrix-compatiblep b)))))))
  'B)

#+nil
(let* ((A (tricopy! 0 (randn '(10 10)) :uo))
       (b (randn '(2 10)))
       (x (trs! 1 A (copy b) :nur :l)))
  #i(x - (/ tricopy! (1, A,\ :d) * b)))
