(in-package #:matlisp)

(declaim (ftype (function ((or stride-accessor coordinate-accessor) &optional index-type) (or index-type index-store-vector)) strides)
	 (ftype (function (stride-accessor) index-type) head))
(definline strides (x &optional idx)
  (declare (type (or stride-accessor coordinate-accessor) x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'strides)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'strides)) (modproj (or idx 0) (order x) nil 0))))
    (t (lvec->list (the index-store-vector (slot-value x 'strides))))))
(definline head (x)
  (declare (type stride-accessor x))
  (slot-value x 'head))
;;
(definline subscripts-check (subs dimensions)
  (declare (type index-store-vector dimensions))
  (macrolet ((check (vectorp)
	       `(let* ((idx (t/store-allocator index-store-vector (length dimensions))))
		  ,@(if vectorp `((declare (type index-store-vector subs))))
		  (loop :for ii :of-type index-type :from 0 :below (length idx)
		     :for si :of-type index-type ,@(if vectorp `(:across) `(:in)) subs
		     :do (setf (aref idx ii) (modproj si (aref dimensions ii) nil nil))
		     :finally (progn (assert (= ii (length idx)) nil 'tensor-index-rank-mismatch) (return idx))))))
    (etypecase subs
      (list (check nil))
      (index-store-vector (check t)))))

(definline stride-hash (idx strides)
  "
  Syntax
  ======
  (STRIDE-HASH IDX STRIDES)

  Purpose
  =======
  Computes the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
  "

  (declare (type index-store-vector idx strides))
  (loop :for cidx :of-type index-type :across idx
     :for sidx :of-type index-type :across strides
     :summing (the index-type (* cidx sidx)) :of-type index-type))
;;
(definline invert-hash (hash sort-index strides dimensions)
  "uniqueness seems to make this O(n); ignoring rigour for now."
  (declare (type index-store-vector dimensions strides sort-index)
	   (type index-type hash))
  (let*-typed ((idx (t/store-allocator index-store-vector (length dimensions))))
    (iter (for ii in-vector sort-index)
	  (let*-typed ((p/i (signum ii)) (ii (abs ii))
		       (si (aref strides ii)) (di (aref dimensions ii))
		       (q (max 0 (min (+ (floor hash si) (if (< p/i 0) 1 0)) di)) :type index-type))
	    (setf (aref idx ii) q)
	    (decf hash (* q (aref strides ii))))
	  (finally (return (if (= hash 0) idx))))))

;;TODO: add a check.
(definline stride-pivot (strides)
  (declare (type index-store-vector strides))
  (letv* ((_ index (sort-index (copy-seq strides) #'> :key #'abs) :type nil index-store-vector)
	  (sindex (copy-seq index)))
    (iter (for idx in-vector index downto 0 with-index i)
	  (setf (aref sindex i) (* idx (if (or (first-time-p) (= (signum (aref strides idx)) (signum (aref strides (aref index (1+ i)))))) 1 -1))))
    sindex))

;;Stride makers.
(macrolet ((defstride (fname col?)
	     `(definline ,fname (dims)
		(declare (type index-store-vector dims))
		(let-typed ((stds (t/store-allocator index-store-vector (length dims)) :type index-store-vector))
		  (very-quickly
		    (iter
		      ,(if col?
			   `(for i from 0 below (length dims))
			   `(for i from (1- (length dims)) downto 0))
		      (declare (type index-type i))
		      (with st = 1) (declare (type index-type st))
		      (let-typed ((d (aref dims i) :type index-type))
			(assert (> d 0) nil 'tensor-invalid-dimension-value :argument i :dimension d)
			(setf (aref stds i) st
			      st (* st d)))
		      (finally (return (values stds st)))))))))
  (defstride make-stride-cmj t)
  (defstride make-stride-rmj nil)
  (definline make-stride (dims)
    (ecase *default-stride-ordering* (:row-major (make-stride-rmj dims)) (:col-major (make-stride-cmj dims)))))
;;
(closer-mop:defmethod initialize-instance :after ((tensor stride-accessor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (let-typed ((dims (dimensions tensor) :type index-store-vector)
		(linearp (vectorp (slot-value tensor 'store))))
      (assert (>= (head tensor) 0) nil 'tensor-invalid-head-value :head (head tensor) :tensor tensor)
      (if (not (slot-boundp tensor 'strides))
	  (letv* ((stds size (make-stride dims) :type index-store-vector index-type))
	    (setf (slot-value tensor 'strides) stds)
	    (when linearp
	      (assert (<= (+ (head tensor) size) (store-size tensor)) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (+ (head tensor) (1- (total-size tensor))) :tensor tensor)))
	  (very-quickly
	    (let-typed ((stds (strides tensor) :type index-store-vector))
	      (loop :for i :of-type index-type :from 0 :below (order tensor)
		 :for sz :of-type index-type := (aref dims 0) :then (the index-type (* sz (aref dims i)))
		 :summing (the index-type (the index-type (* (aref stds i) (1- (aref dims i))))) :into lidx :of-type index-type
		 :do (assert (> (aref dims i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (aref dims i) :tensor tensor)
		 :finally (when linearp
			    (assert (>= (the index-type (store-size tensor)) (the index-type (+ (the index-type (head tensor)) lidx)) 0) nil 'tensor-insufficient-store :store-size (store-size tensor) :max-idx (the index-type (+ (head tensor) lidx)) :tensor tensor)))))))))
;;
(define-tensor-method ref ((x stride-accessor :x) &rest subscripts)
  `(let-typed ((off (+ (head x) (stride-hash (match subscripts
					       ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
					       (_ (subscripts-check (the list subscripts) (dimensions x))))
					     (strides x))) :type index-type))
     (t/store-ref ,(cl :x) (t/store ,(cl :x) x) off)))

(define-tensor-method (setf ref) (value (x stride-accessor :x) &rest subscripts)
  `(let-typed ((off (+ (head x) (stride-hash (match subscripts
					       ((list* (and subs/v (type index-store-vector)) _) (subscripts-check (the index-store-vector subs/v) (dimensions x)))
					       (_ (subscripts-check (the list subscripts) (dimensions x))))
					     (strides x))) :type index-type))
     (t/store-set ,(cl :x) (t/coerce ,(field-type (cl :x)) value) (t/store ,(cl :x) x) off)))
;;
