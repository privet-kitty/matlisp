(in-package #:matlisp)

(definline tb+ (a &optional b)
  "
  Syntax
  ======
  (t+ a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
  "
  (if b
      (cart-etypecase (a b)
	((number number) (cl:+ a b))
	((number tensor) (axpy a nil b))
	((tensor number) (axpy b nil a))
	((tensor tensor) (axpy 1 a b)))
      a))

(definline t+ (&rest objs)
  (reduce #'tb+ objs))
;;
(definline tb- (a &optional b)
  "
  Syntax
  ======
  (t- a b)

  Purpose
  =======
  Create a new matrix which is the sum of A and B.
  A or B (but not both) may be a scalar, in which
  case the addition is element-wise.
"
  (if b
      (cart-etypecase (a b)
	((number number) (cl:- a b))
	((number tensor) (axpy! a nil (scal -1 b)))
	((tensor number) (axpy (cl:- b) nil a))
	((tensor tensor) (axpy -1 b a)))
      (etypecase a
	(number (cl:- a))
	(tensor (scal -1 a)))))

(definline t- (&rest objs)
  (if (cdr objs)
      (reduce #'tb- objs)
      (tb- (car objs))))
;;
(definline tb* (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:* a b))
	;;Scaling
	((number tensor) (scal a b))
	((tensor number) (scal b a))
	;;Matrix, vector/matrix product
	((tensor-matrix tensor-matrix) (gemm 1 a b nil nil))
	((tensor-matrix tensor-vector) (gemv 1 a b nil nil :n))
	((tensor-vector tensor-matrix) (gemv 1 b a nil nil :t))
	;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
	((permutation base-tensor) (permute b a 0))
	((tensor permutation) (permute a b -1))
	;;The correctness of this depends on the left-right order in reduce (foldl).
	((permutation permutation) (compose a b)))
      a))

(defmacro tb*-opt (a b)
  (labels ((op (code)
	     (when (consp code)
	       (case (car code)
		 (htranspose #\C)
		 (transpose #\T)))))
    (if (or (op a) (op b))
	(with-gensyms (ma mb)
	  `(let ((,ma ,(if (op a) (second a) a))
		 (,mb ,(if (op b) (second b) b)))
	     (cart-etypecase (,ma ,mb)
	       ((tensor-matrix tensor-matrix)
		(gemm 1 ,ma ,mb nil nil ,(intern (coerce (list (or (op a) #\N) (or (op b) #\N)) 'string) :keyword)))
	       ((tensor-matrix tensor-vector) ;;The other case involves a complex conjugate.
		(gemv 1 ,ma ,mb nil nil ,(intern (coerce (list (or (op a) #\N)) 'string) :keyword)))
	       ((t t)
		(tb* ,(if (op a) `(,(car a) ,ma) ma) ,(if (op b) `(,(car b) ,mb) mb))))))
	`(tb* ,a ,b))))

(definline t* (&rest objs)
  (reduce #'tb* objs))
;;
(definline tb.* (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:* a b))
	;;Scaling
	(((or number tensor) (or number base-tensor)) (scal a b)))
      a))

(definline t.* (&rest objs)
  (reduce #'tb.* objs))
;;
(definline tb./ (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (cl:/ a b))
	;;Scaling
	(((or number tensor) (or number tensor)) (div b a)))
      (etypecase a
	(number (cl:/ a))
	(tensor (div a 1)))))

(definline t./ (&rest objs)
  (if (cdr objs)
      (reduce #'tb./ objs)
      (tb./ (car objs))))
;;
(defparameter *tensor-contraction-functable* (make-hash-table :test 'equal))
(defgeneric gett! (alpha a b beta c)
  (:method :before (alpha (a tensor) (b tensor) beta (c tensor))
     (assert (and (= (dimensions a -1) (dimensions b 0))
		  (=  (+ (order a) (order b) -2) (order c))
		  (dotimes (i (1- (order a)) t) (unless (= (dimensions a i) (dimensions c i)) (return nil)))
		  (dotimes (i (1- (order b)) t) (unless (= (dimensions b (1+ i)) (dimensions c (+ (order a) i -1))) (return nil))))
	     nil 'tensor-dimension-mismatch)))

(define-tensor-method gett! (alpha (a dense-tensor :x) (b dense-tensor :x) beta (c dense-tensor :x t))
  `(let ((func (or (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
		   (let ((asyms (iter (for i from 0 below (1- (order a))) (collect (gensym (format nil "a_~a" i)))))
			 (bsyms (iter (for i from 1 below (order b)) (collect (gensym (format nil "b_~a" i)))))
			 (sumsym (gensym "idx")))
		     (format t "Generating contraction for orders : (~a, ~a)." (order a) (order b))
		     (setf (gethash (list (order a) (order b) ',(cl a)) *tensor-contraction-functable*)
			   (compile-and-eval `(lambda-einstein (alpha a b c) (,',(cl a)  (ref c ,@asyms ,@bsyms) (* alpha (ref a ,@asyms ,sumsym) (ref b ,sumsym ,@bsyms)) nil)))))))
	 (alpha (t/coerce ,(field-type (cl a)) alpha))
	 (beta (t/coerce ,(field-type (cl a)) beta)))
     (unless (t/f= ,(field-type (cl a)) beta (t/fid* ,(field-type (cl a)))) (scal! beta c))
     (funcall func alpha a b c)
     c))
;;
(definline tb@ (a b)
  (cart-etypecase (a b)
    ((number number) (cl:* a b))
    ;;Scaling
    ((number tensor) (scal a b))
    ((tensor number) (scal b a))
    ;;Matrix, vector/matrix product
    ((vector vector) (dot a b nil))
    ((tensor-matrix tensor-matrix) (gemm 1 a b nil nil))
    ((tensor-matrix tensor-vector) (gemv 1 a b nil nil :n))
    ((tensor-vector tensor-matrix) (gemv 1 b a nil nil :t))
    ((tensor tensor) (gett! 1 a b 1 (zeros (append (butlast (dimensions a t)) (cdr (dimensions b t))) (class-of a))))
    ;;Permutation action on arguments. Left action unpermutes arguments, right action permutes them.
    ;;See tb* for comparison.
    ((permutation tensor) (transpose b (inv a)))
    ((tensor permutation) (transpose a b))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation) (compose a b))))

(definline t@ (&rest objs)
  (reduce #'tb@ objs))
;;
(definline tb/ (b a)
  "Solve x a = b"
  (cart-etypecase (b a)
    ((number number) (cl:/ b a))
    ((tensor number) (scal (cl:/ a) b))
    (((eql nil) (or number permutation (and base-square-matrix blas-numeric-tensor)))
     (inv a))
    (((and tensor-matrix (satisfies blas-tensorp)) (and tensor-square-matrix (satisfies blas-tensorp)))
     (transpose (with-colm (getrs! (getrf! (copy a)) (transpose b) :t))))
    (((and base-vector (satisfies blas-tensorp)) (and tensor-square-matrix (satisfies blas-tensorp)))
     (let ((ret (copy b)))
       (with-colm (getrs! (getrf! (copy a)) (suptensor~ ret 2) :t))
       ret))
    ((standard-tensor permutation)
     (permute b (inv a) -1))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation)
     (compose b (inv a)))))

(definline tb\\ (b a)
  "Solve a x = b"
  (cart-etypecase (b a)
    ((number number) (cl:/ b a))
    ((tensor number) (scal (cl:/ a) b))
    (((eql nil) (or number permutation (and tensor-square-matrix (satisfies blas-tensorp))))
     (inv a))
    (((and tensor-matrix (satisfies blas-tensorp)) (and tensor-square-matrix (satisfies blas-tensorp)))
     (getrs! (getrf! (with-colm (copy a))) (copy b)))
    (((and tensor-vector (satisfies blas-tensorp)) (and tensor-square-matrix (satisfies blas-tensorp)))
     (let ((ret (copy b)))
       (getrs! (getrf! (with-colm (copy a))) (suptensor~ ret 2))
       ret))
    ((dense-tensor permutation)
     (permute b (inv a) 0))
    ;;The correctness of this depends on the left-right order in reduce (foldl).
    ((permutation permutation)
     (compose (inv a) b))))

;;This conflicts semantically with Wedge product.
(defgeneric tb^ (a b)
  (:documentation "Returns the tensor outer product of a and b."))

(defmethod tb^ ((a dense-tensor) b) ;;col-vector
  (orphanize (suptensor~ (scal b a) (1+ (order a)))))
(defmethod tb^ (a (b dense-tensor)) ;;row-vector
  (orphanize (suptensor~ (scal a b) (1+ (order b)) 1)))
(define-tensor-method tb^ ((a dense-tensor :x) (b dense-tensor :x))
  `(cart-etypecase (a b)
     ((tensor-vector tensor-vector)
      (ger 1 a b nil nil))
     ((dense-tensor dense-tensor)
      (let* ((ret (zeros (append (dims a) (dims b)) ',(cl a)))
	     (ret-a (subtensor~ ret (loop :for i :from 0 :below (order ret)
				       :collect (if (< i (order a)) '(nil nil) 0))))
	     (rbstr (subseq (strides ret) (order a)))
	     (sto-b (store b)))
	(iter (for-mod idx from 0 below (dimensions b) with-strides ((of-b (strides b) (head b))
								     (of-r rbstr (head ret))))
	      (setf (slot-value ret-a 'head) of-r)
	      (axpy! (t/store-ref ,(cl b) sto-b of-b) a ret-a))
	ret))))

(definline t^ (&rest objs)
  (reduce #'tb^ objs))
;;
(defgeneric ge== (a b)
  (:method ((a tensor) (b tensor))
    (assert (lvec-eq (dimensions a) (dimensions b)) nil 'tensor-dimension-mismatch)))

(define-tensor-method ge== (a (b dense-tensor :input))
  `(let ((a (t/coerce ,(field-type (cl b)) a))
	 (ret (zeros (dimensions b) '(bit))))
     (dorefs (idx (dimensions b))
	     ((ref.b b :type ,(cl b))
	      (ref.r ret :type ,(tensor 'bit)))
	     (when (t/f= ,(field-type (cl b)) a ref.b) (setf ref.r 1)))
     ret))
(define-tensor-method ge== ((a dense-tensor :x) (b dense-tensor :x))
  `(let ((ret (zeros (dimensions a) '(bit))))
     (dorefs (idx (dimensions a))
	     ((ref.a a :type ,(cl a))
	      (ref.b b :type ,(cl b))
	      (ref.r ret :type ,(tensor 'bit)))
	     (when (t/f= ,(field-type (cl a)) ref.a ref.b) (setf ref.r 1)))
     ret))

(definline tb== (a &optional b)
  (if b
      (cart-etypecase (a b)
	((number number) (if (cl:= a b) 1 0))
	(((or number base-tensor) (or number base-tensor)) (etypecase b (base-tensor (ge== a b)) (number (ge== b a)))))
      1))
