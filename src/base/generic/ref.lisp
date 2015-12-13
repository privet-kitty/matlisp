(in-package #:matlisp)

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
  (defmethod ref ((obj array) &rest subscripts)
    (apply #'aref obj (array-subs obj subscripts)))
  (defmethod (setf ref) (value (obj array) &rest subscripts)
    (apply #'(setf aref) value obj (array-subs obj subscripts))))

(labels ((list-subs (obj subscripts)
	   (let ((subs (etypecase (car subscripts)
			 (number subscripts)
			 (cons (car subscripts))
			 (vector (lvec->list (car subscripts))))))
	     (assert (= (length subs) 1) nil 'invalid-arguments) (setf subs (car subs))
	     (when (< subs 0) (setf subs (modproj subs (length obj))))
	     subs)))
  (defmethod ref ((obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (getf obj (first subscripts)))
      (t (elt obj (list-subs obj subscripts)))))
  (defmethod (setf ref) (value (obj cons) &rest subscripts)
    (cond
      ((and (not (cdr subscripts)) (symbolp (first subscripts))) (setf (getf obj (first subscripts)) value))
      (t (setf (elt obj (list-subs obj subscripts)) value)))))

(defmethod ref :before ((obj hash-table) &rest subscripts)
  (assert (and (first subscripts) (not (cdr subscripts))) nil 'invalid-arguments))

(defmethod ref ((obj hash-table) &rest subscripts)
  (gethash (car subscripts) obj))

(defmethod (setf ref) (value (obj hash-table) &rest subscripts)
  (setf (gethash (car subscripts) obj) value))
;;
