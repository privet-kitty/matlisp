(in-package #:matlisp)

;;Skip for now.

(declaim (ftype (function (coordinate-accessor &optional index-type) (or index-store-matrix list)) indices))

(definline indices (x &optional idx)
  (declare (type coordinate-accessor x))
  (typecase idx
    (null (the index-store-matrix (slot-value x 'indices)))
    (index-type (let-typed ((midx (slot-value x 'indices) :type index-store-matrix)) ;;(array-row-major-index idx 0)
		  (loop :for i :of-type index-type :from 0 :below (array-dimension midx 1) :collect (aref midx idx i))))))

(definline coordinate-indexing (subs tensor)
  (declare (type list subs)
	   (type graph-accessor tensor))
  (let-typed ((hash (stride-indexing subs tensor) :type index-type)
	      (shash (slot-value tensor 'stride-hash) :type index-store-vector))
    (very-quickly (binary-search hash 0 (the index-type (slot-value tensor 'boundary)) shash))))

(define-tensor-method ref ((x coordinate-accessor :x) &rest subscripts)
  `(if-let (idx (coordinate-indexing subscripts x))
     (values (t/store-ref ,(cl x) (t/store ,(cl x) x) (the index-type idx)) t)
     (values (t/fid+ (t/field-type ,(cl x))) nil)))
;;May be keep a stride-hash
