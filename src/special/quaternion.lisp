(in-package #:matlisp)
(in-readtable :infix-dispatch-table)
;;Most of these methods were copied from MuJoCo.
(definline quaternion-vectorp (x)
  (declare (type dense-tensor x))
  (and (= (order x) 1) (= (dimensions x 0) 4) (= (strides x 0) 1)))
(deftype quaternion-vector (&optional (type '* type-p))
  (if type-p
      `(and ,(tensor type) (satisfies quaternion-vectorp))
      `(satisfies quaternion-vectorp)))

(definline r3-vectorp (x)
  (declare (type dense-tensor x))
  (and (= (order x) 1) (= (dimensions x 0) 3) (= (strides x 0) 1)))
(deftype r3-vector (&optional (type '* type-p))
  (if type-p
      `(and ,(tensor type) (satisfies r3-vectorp))
      `(satisfies r3-vectorp)))
;;
(defgeneric r3-cross (qa qb)
  (:method :before ((qa dense-tensor) (qb dense-tensor))
     (assert (and (typep qa 'r3-vector) (typep qb 'r3-vector)) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method r3-cross ((a dense-tensor :x) (b dense-tensor :x))
  `(optimize-expression ((res (zeros 3 ',(cl :x)) :type ,(cl :x) :strides (1))
			 (a a :type ,(cl :x) :strides (1))
			 (b b :type ,(cl :x) :strides (1)))
     #i(res[0] ← a[1]*b[2] - a[2]*b[1],
	res[1] ← a[2]*b[0] - a[0]*b[2],
	res[2] ← a[0]*b[1] - a[1]*b[0])
     res))

(definline r3-cross-ss (a b)
  (declare (type #.(tensor 'double-float) a b))
  (optimize-expression ((res (make-instance ' #.(tensor 'double-float)
					      :dimensions #.(coerce '(3) 'index-store-vector) :strides #.(make-stride (coerce '(3) 'index-store-vector)) :head 0 :store (t/store-allocator #. (tensor 'double-float) 3)) :type #.(tensor 'double-float) :strides (1))
			(a a :type #.(tensor 'double-float) :strides (1))
			(b b :type #.(tensor 'double-float) :strides (1)))
     #i(res[0] ← a[1]*b[2] - a[2]*b[1],
	res[1] ← a[2]*b[0] - a[0]*b[2],
	res[2] ← a[0]*b[1] - a[1]*b[0])
     res))

(definline r3-cross-ss (a b)
  (declare (type #.(tensor 'double-float) a b))
  ;; (make-instance ' #.(tensor 'double-float)
  ;; 		   :dimensions #.(coerce '(3) 'index-store-vector) :strides #.(make-stride (coerce '(3) 'index-store-vector)) :head 0 :store (t/store-allocator #. (tensor 'double-float) 3))
  ;; #+nil 
  (optimize-expression ((res (t/zeros #.(tensor 'double-float) '(3))
			     #+nil(make-instance ' #.(tensor 'double-float)
						   :dimensions (copy-seq #.(coerce '(3) 'index-store-vector)) :strides (make-stride (copy-seq #.(coerce '(3) 'index-store-vector))) :head 0 :store (t/store-allocator #. (tensor 'double-float) 3)) :type #.(tensor 'double-float) :strides (1))
			(a a :type #.(tensor 'double-float) :strides (1))
			(b b :type #.(tensor 'double-float) :strides (1)))
		       #i(res[0] ← a[1]*b[2] - a[2]*b[1],
				   res[1] ← a[2]*b[0] - a[0]*b[2],
				   res[2] ← a[0]*b[1] - a[1]*b[0])
		       res))

;; ;;#+nil
(let ((a (randn 3))
      (b (randn 3))
      (ret 0)
      (dd (idxv 3))
      (ss (idxv 1)))
  (declare (type #.(tensor 'double-float) a b))
  ;;(r3-cross-ss a b)
  ;;(r3-cross a b)
  (time (very-quickly (dotimes (i 1000)
			(r3-cross-ss a b)
			;;(r3-cross-ss a b)
			;;(aref (store (r3-cross-ss a b)) 0)
			;;(incf ret (aref (store (r3-cross-ss a b)) 0))
			#+nil
			(make-instance ' #.(tensor 'double-float)
					 :dimensions dd :strides ss :head 0 :store (t/store-allocator #. (tensor 'double-float) 3))
			;;(make-instance 'thingy :xx 1)
			;;(make-stride dd)
			;;(t/zeros #.(tensor 'double-float) dd)
			#+nil(with-no-init-checks
				 (make-instance ' #.(tensor 'double-float))))))
  ;;(time (very-quickly (dotimes (i 10000) (incf ret (quaternion-x (r3-cross (the #.(quaternion 'double-float) a) (the #.(quaternion 'double-float) b)))))))
  )

(defclass tmp () (a b))
(allocate-instance )

(defmethod sb-pcl::allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (sb-pcl::allocate-standard-instance (sb-pcl::class-wrapper class)))


(let ((ret 0))
  (time (very-quickly (dotimes (i 1000)
			;;(sb-pcl::allocate-standard-instance (sb-pcl::class-wrapper (find-class 'tmp)))
			(allocate-instance #.(find-class 'tmp))
			))))

(make-instance )

;;TODO: remove initialize-instance methods
;;TODO: add compiler-macro to zeros

;; (remmeth #'initialize-instance `(base-accessor) '(:after))
;; (find-method #'initialize-instance :after (mapcar #'find-class `(stride-accessor)))

;;
(defun quat-vector~ (x)
  (declare (type quaternion-vector x))
  (let ((tmp (subtensor~ x nil)))
    (incf (slot-value tmp 'head))
    (setf (aref (dimensions tmp) 0) 3)
    tmp))

;;
(defgeneric quat/ (x)
  (:method :before ((x dense-tensor))
     (assert (typep x 'quaternion-vector) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method quat/ ((x dense-tensor :x))
  `(optimize-expression ((res (zeros 4 ',(cl :x)) :type ,(cl :x) :strides (1))
			 (x x :type ,(cl :x) :strides (1)))
     (t/copy! (,(cl :x) ,(cl :x)) x res)
     #i(res[0] ← - res[0])
     (t/scdi! ,(cl :x) (tb*-opt ,(coerce -1 (field-type (cl :x))) (t/dot ,(cl :x) x x nil nil)) res :scal? nil :numx? t)
     res))
;;
(defgeneric quat* (qa qb)
  (:method :before ((qa dense-tensor) (qb dense-tensor))
     (assert (and (typep qa 'quaternion-vector) (typep qb 'quaternion-vector)) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method quat* ((qa dense-tensor :x) (qb dense-tensor :x))
  `(optimize-expression ((res (zeros 4 ',(cl :x)) :type ,(cl :x) :strides (1))
			 (qa qa :type ,(cl :x) :strides (1))
			 (qb qb :type ,(cl :x) :strides (1)))
     #i(res[0] ← qa[0]*qb[0] - qa[1]*qb[1] - qa[2]*qb[2] - qa[3]*qb[3],
	res[1] ← qa[0]*qb[1] + qa[1]*qb[0] + qa[2]*qb[3] - qa[3]*qb[2],
	res[2] ← qa[0]*qb[2] - qa[1]*qb[3] + qa[2]*qb[0] + qa[3]*qb[1],
	res[3] ← qa[0]*qb[3] + qa[1]*qb[2] - qa[2]*qb[1] + qa[3]*qb[0])
     res))
;;
(defgeneric quat->mat (q)
  (:method :before ((q dense-tensor)) (assert (typep q 'quaternion-vector) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method quat->mat ((q dense-tensor :x))
  `(optimize-expression ((res (zeros (list 3 3) ',(cl :x)) :type ,(cl :x))
			 (quat q :type ,(cl :x) :strides (1)))
     (let-typed ((q00 #i(quat[0]*quat[0]) :type ,(field-type (cl :x)))
		 (q01 #i(quat[0]*quat[1]) :type ,(field-type (cl :x)))
		 (q02 #i(quat[0]*quat[2]) :type ,(field-type (cl :x)))
		 (q03 #i(quat[0]*quat[3]) :type ,(field-type (cl :x)))
		 (q11 #i(quat[1]*quat[1]) :type ,(field-type (cl :x)))
		 (q12 #i(quat[1]*quat[2]) :type ,(field-type (cl :x)))
		 (q13 #i(quat[1]*quat[3]) :type ,(field-type (cl :x)))
		 (q22 #i(quat[2]*quat[2]) :type ,(field-type (cl :x)))
		 (q23 #i(quat[2]*quat[3]) :type ,(field-type (cl :x)))
		 (q33 #i(quat[3]*quat[3]) :type ,(field-type (cl :x))))
       #i(res[0, 0] ← q00 + q11 - q22 - q33,
	  res[1, 1] ← q00 - q11 + q22 - q33,
	  res[2, 2] ← q00 - q11 - q22 + q33,
	  res[0, 1] ← 2 * (q12 - q03),
	  res[0, 2] ← 2 * (q13 + q02),
	  res[1, 0] ← 2 * (q12 + q03),
	  res[1, 2] ← 2 * (q23 - q01),
	  res[2, 0] ← 2 * (q13 - q02),
	  res[2, 1] ← 2 * (q23 + q01)))
     res))
;;log map
(defgeneric quat-log (dq)
  (:method :before ((dq dense-tensor))
     (assert (typep dq 'quaternion-vector) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method quat-log ((dq dense-tensor :x))
  `(optimize-expression ((q dq :type ,(cl :x)))
     (letv* ((q3 (quat-vector~ q) :type ,(cl :x))
	     (norm (sqrt (t/dot ,(cl :x) q3 q3 t nil)) :type ,(field-type (cl :x)))
	     (res (zeros 3 ',(cl :x)) :type ,(cl :x))
	     (theta (* ,(coerce 2 (field-type (cl :x))) (atan norm #i(q[0]))) :type ,(field-type (cl :x))))
       (if (> theta ,(coerce pi (field-type (cl :x)))) (setf theta (cl:- theta)))
       (if (< (t/fid+ ,(field-type (cl :x))) norm)
	   (t/scdi! ,(cl :x) (/ theta norm) (copy! q3 res) :scal? t :numx? t))
       res)))
;;
(defgeneric quat-exp (angle axis)
  (:method :before (angle (axis dense-tensor))
     (assert (typep axis 'r3-vector) nil))
  (:generic-function-class tensor-method-generator))
(define-tensor-method quat-exp (angle (axis dense-tensor :x))
  `(optimize-expression ((axis axis :type ,(cl :x))
			 (quat (zeros 4 ',(cl :x)) :type ,(cl :x))
			 (q3 (quat-vector~ quat) :type ,(cl :x)))
     (letv* ((norm (sqrt (t/dot ,(cl :x) axis axis t nil)) :type ,(field-type (cl :x)))
	     (angle (tb* (coerce angle ',(field-type (cl :x))) norm) :type ,(field-type (cl :x)))
	     (sincos (exp (complex (t/fid+ ,(field-type (cl :x))) (tb/ angle ,(coerce 2 (field-type (cl :x)))))) :type (complex ,(field-type (cl :x)))))
       (if (< (t/fid+ ,(field-type (cl :x))) norm)
	   (scal! (tb/ (cl:imagpart sincos) norm) (copy! axis q3)))
       #i(quat[0] ← \ (cl:realpart sincos))
       quat)))
;;
#+nil
(defun spherical-quat (delta phi theta)
  (declare (type double-float delta phi theta))
  (optimize-expression ((q (zeros (list 4) (tensor 'double-float)) :type #.(tensor 'double-float)))
    (with-memoization ()
      #i(q[0] ← memoizing(cos(delta/2d0)),
	 q[1] ← memoizing(sin(theta)) * memoizing(cos(phi)) * memoizing(sin(delta/2d0)),
	 q[2] ← memoizing(sin(theta)) * memoizing(sin(phi)) * memoizing(sin(delta/2d0)),
	 q[3] ← memoizing(cos(theta)) * memoizing(sin(delta/2d0)),
	 q))))
