(in-package #:matlisp)

;;Skip for now.
(declaim (ftype (function (coordinate-accessor &optional index-type) (or index-store-matrix index-store-vector)) indices))

(definline indices (x &optional idx)
  (declare (type coordinate-accessor x))
  (typecase idx
    (null (the index-store-matrix (slot-value x 'indices)))
    (index-type (let*-typed ((midx (slot-value x 'indices) :type index-store-matrix)
			     (order (array-dimension midx 1) :type index-type))
		  (lvec-copy order midx (* idx order) (t/store-allocator index-store-vector order) 0 :key #'row-major-aref :lock #'(setf aref))))))

(definline coordinate-indexing (idx tensor)
  (declare (type index-store-vector idx) (type coordinate-accessor tensor))
  (let*-typed ((hash-value (stride-hash idx (strides tensor)) :type index-type)
	       (hash-vector (slot-value tensor 'stride-hash) :type index-store-vector))
    (very-quickly (binary-search hash-value 0 (the index-type (slot-value tensor 'tail)) hash-vector))))

(define-tensor-method ref ((x coordinate-tensor :x) &rest subscripts)
  `(if-let ((idx (coordinate-indexing (match subscripts
					 ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
					 (_ (subscripts-check (the list subscripts) (dimensions x))))
				       x)))
     (values (t/store-ref ,(cl :x) (t/store ,(cl :x) x) (the index-type idx)) t)
     (values (t/fid+ (t/field-type ,(cl :x))) nil)))

(define-tensor-method (setf ref) (value (x coordinate-tensor :x) &rest subscripts)
  `(letv* ((subs/v (match subscripts
		     ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
		     (_ (subscripts-check (the list subscripts) (dimensions x))))
		   :type index-store-vector)
	   (m lb (coordinate-indexing subs/v x)))
     (if m
	 (values (setf (t/store-ref ,(cl :x) (t/store ,(cl :x) x) (the index-type m)) (t/coerce ,(field-type (cl :x)) value)) t)
	 (if *sparse-tensor-realloc-on-setf*
	     (with-memoization ()
	       (memoizing lb :type index-type :bind lb :global t)
	       (memoizing (- (memoizing (total-size x) :type index-type) lb) :type index-type :bind r-len :global t)
	       (memoizing (order x) :type index-type :global t)
	       (if (< (length (memoizing (slot-value x 'stride-hash) :type index-store-vector :bind stride-hash :global t)) (memoizing (store-size x) :global t))
		   (progn ; very-quickly
		     (lvec-copy (* r-len (memoizing (order x) :global t))
				(memoizing (indices x) :type index-store-matrix :global t) (* lb (memoizing (order x)))
				(memoizing (indices x) :global t) (* (1+ lb) (memoizing (order x) :global t))
				:key #'row-major-aref :lock #'(setf row-major-aref))
		     (lvec-copy r-len stride-hash lb stride-hash (1+ lb) :key #'aref :lock #'(setf aref))
		     (lvec-copy r-len
				(memoizing (t/store ,(cl :x) x) :type ,(store-type (cl :x)) :global t) lb
				(memoizing (t/store ,(cl :x) x) :global t) (1+ lb)
				:key #'(lambda (a_ i_) (declare (index-type i_)) (t/store-ref ,(cl :x) a_ i_))
				:lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t/store-set ,(cl :x) v_ a_ i_))))
		   (let*-typed ((ss (+ (memoizing (store-size x) :global t) *default-sparse-store-increment*))
				(idx-new (t/store-allocator index-store-matrix (list ss (memoizing (order x) :global t))) :type index-store-matrix)
				(hsh-new (t/store-allocator index-store-vector ss) :type index-store-vector)
				(sto-new (t/store-allocator ,(cl :x) ss) :type ,(store-type (cl :x))))
		     (progn ;very-quickly
		       ;;Index
		       (lvec-copy (* lb (memoizing (order x) :global t))
				  (memoizing (indices x) :global t) 0 idx-new 0
				  :key #'row-major-aref :lock #'(setf row-major-aref))
		       (lvec-copy (* r-len (memoizing (order x) :global t))
				  (memoizing (indices x) :global t) (* lb (memoizing (order x) :global t))
				  idx-new (* (1+ lb) (memoizing (order x) :global t))
				  :key #'row-major-aref :lock #'(setf row-major-aref))
		       ;;Hash
		       (lvec-copy lb stride-hash 0 hsh-new 0 :key #'aref :lock #'(setf aref))
		       (lvec-copy r-len stride-hash lb hsh-new (1+ lb) :key #'aref :lock #'(setf aref))
		       ;;Store
		       (lvec-copy lb (memoizing (t/store ,(cl :x) x) :global t) 0 sto-new 0
				  :key #'(lambda (a_ i_) (declare (index-type i_)) (t/store-ref ,(cl :x) a_ i_))
				  :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t/store-set ,(cl :x) v_ a_ i_)))
		       (lvec-copy r-len (memoizing (t/store ,(cl :x) x) :global t) lb  sto-new (1+ lb)
				  :key #'(lambda (a_ i_) (declare (index-type i_)) (t/store-ref ,(cl :x) a_ i_))
				  :lock #'(lambda (v_ a_ i_) (declare (type index-type i_) (type ,(field-type (cl :x)) v_)) (t/store-set ,(cl :x) v_ a_ i_))))
		     (setf (slot-value x 'indices) idx-new (slot-value x 'stride-hash) hsh-new (slot-value x 'store) sto-new)))
	       (lvec-copy (memoizing (order x) :global t) subs/v 0 (indices x) (* lb (memoizing (order x) :global t)))
	       (values (setf
			(aref (slot-value x 'stride-hash) lb) (stride-hash subs/v (strides x))
			(t/store-ref ,(cl :x) (t/store ,(cl :x) x) lb) (t/coerce ,(field-type (cl :x)) value))
		       nil))
	     (error "missing entry in the sparse matrix ~a" subs/v)))))

;; (remmeth #'zeros-generic '(list (eql #.(tensor 'double-float 'coordinate-tensor))))

;; (let ((ret (zeros '(2 2) (tensor 'double-float 'coordinate-tensor) 4)))
;;   ret)

;; (let ((ret (zeros '(10 10) (tensor 'double-float 'coordinate-tensor) 4)))
;;   (indices ret)
;;   )

;; (copy! (adlist->graph '((0) (1 0)) (tensor 'double-float 'graph-tensor)))

#+nil
(let ((ret (zeros '(4 4) (tensor 'double-float 'coordinate-tensor) 4))
      (*sparse-tensor-realloc-on-setf* t))
  (copy! (display-graph (primal-graph '((<- 1 0) (<- 2 1) (<- 3 2) (<- 0 3)) (tensor 'double-float 'graph-tensor))) ret)
  #+nil
  (t/copy! (#.(tensor 'double-float 'graph-tensor) #.(tensor 'double-float 'coordinate-tensor))
	   (primal-graph '((0 1) (1 2)) (tensor 'double-float 'graph-tensor)) ret)
  (setf (ref ret 1 1) pi)
  (indices ret))
