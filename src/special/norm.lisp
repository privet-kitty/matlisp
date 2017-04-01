(in-package :matlisp)

(closer-mop:defgeneric norm (vec &optional n)
  (:generic-function-class tensor-method-generator))
(define-tensor-method norm ((vec dense-tensor :x) &optional (n 2))
  (let ((rtype (field-type (realified-tensor (cl :x)))))
    `(ematch n
       ;;Element-wise
       ((and (type cl:real) (guard n_ (<= 1 n_)))
	(expt 
	 (fold-tensor nil ((ref vec :type ,(cl :x))
			   (acc (t/fid+ ,rtype) :type ,rtype))
	   (t/f+ ,rtype acc (cl:expt (cl:abs ref) n)))
	 (/ n)))
       (:sup
	(fold-tensor nil ((ref vec :type ,(cl :x))
			  (sup (t/fid+ ,rtype) :type ,rtype))
	  (max sup (cl:abs ref))))
       ;;L-ijk...       
       ((and (list* :L (and p (or :sup (and (type cl:real) (guard p_ (<= 1 p_))))) args))
	(if args
	    (let ((nrm (zeros (subseq (dimensions vec) 1) ',(realified-tensor (cl :x))))
		  (sl (subtensor~ vec (list* '(nil nil) (make-list (1- (order vec)) :initial-element 0)))))
	      (with-memoization ()
		(iter (for-mod idx from 0 below (dimensions nrm) with-iterator ((:stride ((of-nrm (strides nrm) (head nrm))
											  (of-sl (subseq (strides vec) 1) (head sl))))))
		      (setf
		       (slot-value sl 'head) of-sl
		       (t/store-ref ,(realified-tensor (cl :x)) (memoizing (store nrm) :type ,(store-type (realified-tensor (cl :x))) :global t) of-nrm) (norm sl p))))	      
	      (norm nrm (list* :L args)))
	    (norm vec p)))
       ;;Schatten
       ((and (list :schatten (and (type cl:real) (guard p (<= 1 p)))) (guard _ (typep vec 'tensor-matrix)))
	(norm (svd vec :nn) p))
       ((and (list :schatten :sup) (guard _ (typep vec 'tensor-matrix))) (ref (svd vec :nn) 0))
       ;;Operator
       ((and (list :operator (and p (or 1 2 :sup))) (guard _ (typep vec 'tensor-matrix)))
	(ecase p
	  (1 (norm vec '(:L 1 :sup)))
	  (2 (norm vec '(:schatten :sup)))
	  (:sup (norm (transpose~ vec) '(:operator 1))))))))
;;
(closer-mop:defgeneric t:max (object &optional key)
  (:generic-function-class tensor-method-generator))
(define-tensor-method t:max ((vec dense-tensor :x) &optional key)
  `(let* ((argmax (t/store-allocator index-store-vector (order vec))))
     (values
      (if key
	  (fold-tensor idx ((refx vec :type ,(cl :x))
			    (max (funcall key (ref vec argmax)) :type cl:real))
	    (let ((keyx (funcall key refx)))
	      (when (< max keyx) (copy! idx argmax) keyx)))
	  (fold-tensor idx ((refx vec :type ,(cl :x))
			    (max (ref vec argmax) :type ,(field-type (cl :x))))
	      (when (< max refx) (copy! idx argmax) refx)))
      argmax)))

(closer-mop:defgeneric t:min (vec &optional key)
  (:generic-function-class tensor-method-generator))
(define-tensor-method t:min ((vec dense-tensor :x) &optional key)
  `(let* ((argmin (t/store-allocator index-store-vector (order vec))))
     (values
      (if key
	  (fold-tensor idx ((refx vec :type ,(cl :x))
			    (min (funcall key (ref vec argmin)) :type cl:real))
	    (let ((keyx (funcall key refx)))
	      (when (> min keyx) (copy! idx argmin) keyx)))
	  (fold-tensor idx ((refx vec :type ,(cl :x))
			    (min (ref vec argmin) :type ,(field-type (cl :x))))
	    (when (> min refx) (copy! idx argmin) refx)))
      argmin)))
;;
