(in-package #:matlisp)

;;

;;Is it a tensor, is a store ? It is both!

(defmethod ref ((tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod ref ((tensor ,clname) &rest subscripts)
	(let ((subs (if (numberp (car subscripts)) subscripts (car subscripts))))
	  (t/store-ref ,clname (store tensor) (store-indexing subs tensor)))))
    (apply #'ref (cons tensor subscripts))))

(defmethod (setf ref) (value (tensor standard-tensor) &rest subscripts)
  (let ((clname (class-name (class-of tensor))))
    (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
    (compile-and-eval
     `(defmethod (setf ref) (value (tensor ,clname) &rest subscripts)
	(let* ((subs (if (numberp (car subscripts)) subscripts (car subscripts)))
	       (idx (store-indexing subs tensor))
	       (sto (store tensor)))
	  (t/store-set ,clname (t/coerce ,(field-type clname) value) sto idx)
	  (t/store-ref ,clname sto idx))))
    (setf (ref tensor (if (numberp (car subscripts)) subscripts (car subscripts))) value)))

;;
(defmethod subtensor~ ((tensor standard-tensor) (subscripts list))
  (multiple-value-bind (hd dims stds) (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))
    (cond
      ((not hd) nil)
      ((not dims) (if subscripts
		      (store-ref tensor hd)
		      (with-no-init-checks
			  (make-instance (class-of tensor)
					 :head (head tensor)
					 :dimensions (copy-seq (dimensions tensor))
					 :strides (copy-seq (strides tensor))
					 :store (store tensor)
					 :parent-tensor tensor))))
      (t (with-no-init-checks
	     (make-instance (class-of tensor)
			    :head (+ hd (head tensor))
			    :dimensions (make-index-store dims)
			    :strides (make-index-store stds)
			    :store (store tensor)
			    :parent-tensor tensor))))))

(defmethod suptensor~ ((ten standard-tensor) ord &optional (start 0))
  (declare (type index-type ord start))
  (if (= (order ten) ord) ten
      (let* ((tord (order ten)))
	(with-no-init-checks
	    (make-instance (class-of ten)
			   :dimensions (make-index-store
					(nconc (make-list start :initial-element 1)
					       (lvec->list (dimensions ten))
					       (make-list (- ord tord start) :initial-element 1)))
			   :strides (make-index-store
				     (nconc (make-list start :initial-element (size ten))
					    (lvec->list (strides ten))
					    (make-list (- ord tord start) :initial-element (size ten))))
			   :head (head ten)
			   :store (store ten)
			   :parent-tensor ten)))))

(defmethod reshape! :before ((tensor standard-tensor) (dims cons))
  (assert (iter (for s in-vector (strides tensor))
		(unless (> (* s (strides tensor 0)) 0) (return nil))
		(finally (return t)))
	  nil 'tensor-error :message "strides are not of the same sign." :tensor tensor)
  (assert (<= (iter (for i in dims) (multiplying i)) (store-size tensor)) nil 'tensor-insufficient-store))

(defmethod reshape! ((ten standard-tensor) (dims cons))
  (let ((idim (make-index-store dims)))
    (setf (slot-value ten 'dimensions) idim
	  (slot-value ten 'strides) (let ((strd (make-stride idim)))
				      (when (< (strides ten 0) 0)
					(iter (for i from 0 below (length strd))
					      (setf (aref strd i) (- (aref strd i)))))
				      strd))
    ten))
