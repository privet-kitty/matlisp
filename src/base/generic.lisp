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
  (:method  :before ((x array) (y tensor))
     (assert (equal (array-dimensions x) (dimensions y t)) nil 'dimension-mismatch))
  (:method :before ((x tensor) (y array))
     (assert (equal (array-dimensions y) (dimensions x t)) nil 'dimension-mismatch))  
  (:method :before ((x cons) (y cons))
     (assert (= (length x) (length y)) nil 'dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(definline copy (obj &optional type)
  (copy! obj (etypecase type (symbol type) (standard-class (class-name type)))))

(closer-mop:defmethod copy! ((num number) (type symbol))
  (if type (coerce num type) num))

(closer-mop:defmethod copy! ((from cons) (to cons))
  (do ((flst from (cdr flst))
       (tlst to (cdr tlst)))
      ((or (null flst) (null tlst)))
    (cart-etypecase ((car flst) (car tlst))
      ((atom atom) (setf (car tlst) (car flst)))
      ((cons cons) (copy! (car flst) (car tlst)))))
  to)

(closer-mop:defmethod copy! ((lst cons) (type symbol))
  (labels ((list-dimensions (lst)
	     (if (atom lst) nil
		 (cons (length lst) (list-dimensions (car lst))))))
    (cond
      ((member type '(list cons nil)) (copy-tree lst))
      ((eql type 'vector) (make-array (length lst) :initial-contents lst))
      ((eql type 'array) (make-array (list-dimensions lst) :initial-contents lst))
      ((subtypep type 'tensor) (copy! lst (zeros (list-dimensions lst) type)))
      (t (error "don't know how to copy a list to type ~a" type)))))

(closer-mop:defmethod copy! ((from t) (to cons))
  (labels ((mapcar! (f lst)
	     (do ((lst* lst (cdr lst*)))
		 ((null lst*))
	       (setf (car lst*) (funcall f (car lst*))))
	     lst))
    (maptree-eki #'(lambda (x) (if (atom x) from (values x #'mapcar!))) to)))

(closer-mop:defmethod copy! ((x cons) (y tensor))
  (copy! (copy x 'array) y))

(closer-mop:defmethod copy! ((from array) (to array))
  (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) '(simple-array index-type (*)))))))))
	(setf (row-major-aref to of-x) (row-major-aref from of-x)))
  to)

(closer-mop:defmethod copy! ((from t) (to array))
  (iter (for-mod idx from 0 below (array-dimensions to) with-iterator ((:stride ((of-x (make-stride-rmj (coerce (array-dimensions to) 'index-store-vector)))))))
	(setf (row-major-aref to of-x) from))
  to)

(closer-mop:defmethod copy! ((arr array) (type symbol))
  (cond
    ((member type '(array nil)) (copy! arr (make-array (array-dimensions arr) :element-type (array-element-type arr))))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (array-rank arr)) (apply #'aref arr idx)
		      (loop :for i :from 0 :below (array-dimension arr n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree arr nil)))
    ((subtypep type 'tensor) (copy! arr (zeros (array-dimensions arr) type)))
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

;;
(labels ((array-subs (obj subscripts)
	   (let ((subs (etypecase (car subscripts)
			 (number subscripts)
			 (cons (car subscripts))
			 (vector (lvec->list (car subscripts))))))
	     (iter (for s on subs)
		   (for i first 0 then (1+ i))
		   (when (< (car s) 0)
		     (rplaca s (modproj (car s) (array-dimension obj i) nil))))
	     subs)))
  (closer-mop:defmethod ref ((obj array) &rest subscripts)
    (apply #'aref obj (array-subs obj subscripts)))
  (closer-mop:defmethod (setf ref) (value (obj array) &rest subscripts)
    (apply #'(setf aref) value obj (array-subs obj subscripts))))

(labels ((list-subs (obj subscripts)
	   (let ((subs (etypecase (car subscripts)
			 (number subscripts)
			 (cons (car subscripts))
			 (vector (lvec->list (car subscripts))))))
	     (assert (= (length subs) 1) nil 'invalid-arguments) (setf subs (car subs))
	     (when (< subs 0) (setf subs (modproj subs (length obj))))
	     subs)))
  (closer-mop:defmethod ref ((obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (getf obj (first subscripts)))
      (t (elt obj (list-subs obj subscripts)))))
  (closer-mop:defmethod (setf ref) (value (obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (setf (getf obj (first subscripts)) value))
      (t (setf (elt obj (list-subs obj subscripts)) value)))))

(closer-mop:defmethod ref :before ((obj hash-table) &rest subscripts)
  (assert (and (first subscripts) (not (cdr subscripts))) nil 'invalid-arguments))

(closer-mop:defmethod ref ((obj hash-table) &rest subscripts)
  (gethash (car subscripts) obj))

(closer-mop:defmethod (setf ref) (value (obj hash-table) &rest subscripts)
  (setf (gethash (car subscripts) obj) value))
