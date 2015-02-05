(in-package #:matlisp)

;;
(defclass compressed-sparse-matrix (sparse-tensor)
  ((transpose? :initform nil :initarg :transpose? :reader transpose? :type boolean
	       :documentation "If NIL the matrix is in CSC, else if T, then matrix is CSR.")
   (neighbour-start :initarg :neighbour-start :reader neighbour-start :type index-store-vector
		    :documentation "Start index for ids and store.")
   (neighbour-id :initarg :neighbour-id :reader neighbour-id :type index-store-vector
		 :documentation "Row id.")))
(declaim (ftype (function (compressed-sparse-matrix) index-store-vector) neighbour-start neighbour-id))

(definline binary-search (val lb ub vec)
  (declare (type index-type lb ub))
  (unless (or (= lb ub) (< val (aref vec lb)) (> val (aref vec (1- ub))))
    (very-quickly
      (loop :for j :of-type index-type := (floor (+ lb ub) 2)
	 :repeat #.(ceiling (log array-dimension-limit 2))
	 :do (cond ((= (aref vec j) val) (return j))
		   ((>= lb (1- ub)) (return))
		   (t (if (< val (aref vec j))
			  (setf ub j)
			  (setf lb (1+ j)))))))))


;;Templates
(deft/method t/store-allocator (cl compressed-sparse-matrix) (size &optional nz)
  (let ((sto-type (store-element-type cl)))
    `(destructuring-bind (nr nc) ,size
       (let ((nz (or ,nz (min (ceiling (* nr nc *default-sparsity*)) *max-sparse-size*))))
	 (list
	  (allocate-index-store nz)
	  (make-array (t/compute-store-size ,cl nz) :element-type ',sto-type :initial-element ,(if (subtypep sto-type 'number) `(t/fid+ ,sto-type) nil)))))))

(deft/method t/compute-store-size (sym compressed-sparse-matrix) (size)
  size)
;;
(deft/method t/store-type (sym compressed-sparse-matrix) (&optional (size '*))
  `(simple-array ,(store-element-type sym) (,size)))

(deft/method t/store-ref (sym compressed-sparse-matrix) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed-store")
  `(aref (the ,(store-type sym) ,store) (the index-type ,(car idx))))

(deft/method t/store-set (sym compressed-sparse-matrix) (value store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for compressed store")
  `(setf (aref (the ,(store-type sym) ,store) (the index-type ,(car idx))) (the ,(field-type sym) ,value)))

(deft/method t/store-size (sym compressed-sparse-matrix) (ele)
  `(length ,ele))

(deft/method t/store-element-type (sym compressed-sparse-matrix) ()
  (macroexpand `(t/field-type ,sym)))
