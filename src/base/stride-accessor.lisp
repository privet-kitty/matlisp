(in-package #:matlisp)

(declaim (ftype (function ((or stride-accessor coordinate-accessor) &optional index-type) (or index-type index-store-vector)) strides)
	 (ftype (function (stride-accessor) index-type) head))
(definline strides (x &optional idx)
  (declare (type stride-accessor x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'strides)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'strides)) (modproj (or idx 0) (order x) nil 0))))
    (t (lvec->list (the index-store-vector (slot-value x 'strides))))))
(definline head (x)
  (declare (type stride-accessor x))
  (slot-value x 'head))
;;

(definline stride-indexing (idx tensor)
"
  Syntax
  ======
  (STRIDE-INDEXING IDX TENSOR)

  Purpose
  =======
  Does error checking to make sure idx is not out of bounds.
  Returns the sum:

    length(STRIDES)
       __
  HD + \  STRIDE  * IDX
       /_        i      i
     i = 0
"
  (declare (type list idx)
	   (type (or stride-accessor coordinate-accessor) tensor))
  (loop :for cidx :of-type index-type :in idx
     :for i :of-type index-type := 0 :then (1+ i)
     :for d :across (dimensions tensor)
     :for s :across (strides tensor)
     :with sto-idx :of-type index-type := (head tensor)
     :do (progn
	   (assert (< (1- (- d)) cidx d) nil 'tensor-index-out-of-bounds :argument i :index cidx :dimension d)
	   (incf sto-idx (the index-type (* s (if (< cidx 0) (mod cidx d) cidx)))))
     :finally (progn
		(assert (= (1+ i) (order tensor)) nil 'tensor-index-rank-mismatch :index-rank (1+ i) :rank (order tensor))
		(return sto-idx))))

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
(defmethod initialize-instance :after ((tensor stride-accessor) &rest initargs)
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
  `(t/store-ref ,(cl x) (t/store ,(cl x) x) (stride-indexing subscripts x)))

(define-tensor-method (setf ref) (value (x stride-accessor :x) &rest subscripts)
  `(t/store-set ,(cl x) (t/coerce ,(field-type (cl x)) value) (t/store ,(cl x) x) (stride-indexing subscripts x)))
;;
