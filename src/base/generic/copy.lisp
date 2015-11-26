(in-package #:matlisp)

(closer-mop:defgeneric copy! (from to)
  (:documentation
   "
  Syntax
  ======
  (COPY! x y)

  Purpose
  =======
  Copies the contents of X into Y. Returns Y.
")
  (:method :before ((x array) (y array))
	   (assert (equal (array-dimensions x) (array-dimensions y)) nil 'dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(defmethod copy! ((from cons) (to cons))
  (do ((flst from (cdr flst))
       (tlst to (cdr tlst)))
      ((or (null flst) (null tlst)))
    (setf (car tlst) (car flst)))
  to)

(defmethod copy! ((from t) (to cons))
  (mapl #'(lambda (lst) (rplaca lst from)) to)
  to)

(defmethod copy! ((from array) (to array))
  (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) '(simple-array index-type (*)))))))))
	(setf (row-major-aref to of-x) (row-major-aref from of-x)))
  to)

(defmethod copy! ((from t) (to array))
  (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) 'index-store-vector)))))))
	(setf (row-major-aref to of-x) from))
  to)

;;
(defmethod copy! :before ((x array) (y tensor))
  (assert (equal (array-dimensions x) (dimensions y t)) nil 'dimension-mismatch))
(defmethod copy! :before ((x tensor) (y array))
  (assert (equal (array-dimensions y) (dimensions x t)) nil 'dimension-mismatch))

(defmethod copy! ((x cons) (y tensor))
  ;;You seriously weren't expecting efficiency were you :) ?
  (let ((arr (make-array (list-dimensions x) :initial-contents x)))
    (copy! arr y)))
;;
(defgeneric copy-generic (object type)
  (:documentation
   "
  Syntax
  ======
  (COPY-GENERIC x type)

  Purpose
  =======
  Return a copy of X coerced to TYPE"))

(definline copy (obj &optional type)
  (copy-generic obj type))

(defmethod copy-generic ((num number) type)
  (if type (coerce num type) num))

(defmethod copy-generic ((lst cons) type)
  (cond
    ((member type '(list cons nil)) (copy-tree lst))
    ((eql type 'vector) (make-array (length lst) :initial-contents lst))
    ((eql type 'array)
     (make-array (list-dimensions lst) :initial-contents lst))
    ((subtypep type 'tensor)
     (let ((ret (zeros (list-dimensions lst) type)))
       (copy! lst ret)))
    (t (error "don't know how to copy a list to type ~a" type))))

(defmethod copy-generic ((arr array) type)
  (cond
    ((member type '(array nil))
     (let ((ret (make-array (array-dimensions arr) :element-type (array-element-type arr))))
       (copy! arr ret)))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (array-rank arr)) (apply #'aref arr idx)
		      (loop :for i :from 0 :below (array-dimension arr n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree arr nil)))
    ((subtypep type 'tensor)
     (let ((ret (zeros (array-dimensions arr) type)))
       (copy! arr ret)))
    (t (error "don't know how to copy a list to type ~a" type))))
;;

(closer-mop:defgeneric swap! (x y)
  (:documentation
"
  Sytnax
  ======
  (SWAP! x y)

  Purpose
  =======
  Given tensors X,Y, performs:

	      X <-> Y

  and returns Y.

  X, Y must have the same dimensions.
")
  (:generic-function-class tensor-method-generator))
