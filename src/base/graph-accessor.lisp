(in-package #:matlisp)

(definline fence (g &optional idx)
  (declare (type graph-accessor g))
  (typecase idx
    (null (the index-store-vector (slot-value g 'fence)))
    (index-type (let*-typed ((f (slot-value g 'fence) :type index-store-vector)
			     (idx (modproj (or idx 0) (1- (length f)) nil 0) :type index-type))
		  (values (aref f idx) (aref f (1+ idx)))))
    (t (lvec->list (the index-store-vector (slot-value g 'fence))))))

(definline δ-I (g &optional i j)
  (declare (type graph-accessor g))
  (cart-etypecase (i j)
    ((null null) (the index-store-vector (slot-value g 'neighbours)))
    ((index-type boolean)
     (if j
	 (letv* ((l r (fence g i))
		 (nn (slot-value g 'neighbours) :type index-store-vector))
	   (loop :for ii :from l :below r :collect (aref nn ii)))
	 (aref (the index-store-vector (slot-value g 'neighbours)) i)))
    ((index-type index-type)
     (letv* ((l r (fence g i))
	     (nn (slot-value g 'neighbours) :type index-store-vector))
       (very-quickly (binary-search j l r nn))))))

(definline |δ-I| (g i)
  (letv* ((l r (fence g (the index-type i)))) (- r l)))
;;
(definline graph-indexing (subs tensor)
  (declare (type list subs)
	   (type graph-accessor tensor))
  (letv* (((i j) (listify-subscripts subs) :type (index-type index-type)))
    (when (slot-value tensor 'transposep) (rotatef i j))
    (letv* ((l r (fence tensor j)))
      (very-quickly (binary-search i l r (δ-i tensor))))))

(define-tensor-method ref ((x graph-accessor :x) &rest subscripts)
  `(if-let (idx (graph-indexing (listify-subscripts subscripts) x))
     (values (t/store-ref ,(cl x) (t/store ,(cl x) x) (the index-type idx)) t)
     (values (t/fid+ (t/field-type ,(cl x))) nil)))

(define-tensor-method (setf ref) (value (x graph-accessor :x) &rest subscripts)
  `(letv* ((subscripts (listify-subscripts subscripts))
	   ((r c) subscripts :type (index-type index-type))
	   (idx lb (graph-indexing subscripts x)))
     (when (slot-value x 'transposep) (rotatef r c))
     (unless idx
       (letv* ((δg (δ-i x) :type index-store-vector)
	       (sto (t/store ,(cl x) x) :type ,(store-type (cl x))))
	 (declare (type index-type lb))
	 ,@(flet ((code (sfr sto δfr δto)
			`(very-quickly
			   ,@(unless (and (eql δfr δto) (eql sto sfr))
				     `((loop :for i :of-type index-type :from 0 :below lb
					  :do (setf (aref ,δto i) (aref ,δfr i)
						    (t/store-ref ,(cl x) ,sto i) (t/store-ref ,(cl x) ,sfr i)))))
			   (loop :for i :from lb :below (nth-value 1 (fence x -1))
			      :do (setf (aref ,δto (1+ i)) (aref ,δfr i)
					(t/store-ref ,(cl x) ,sto (1+ i)) (t/store-ref ,(cl x) ,sfr i))))))
		 `((if (> (store-size x) (nth-value 1 (fence x -1)))
		       ,(code 'sto 'sto 'δg 'δg)
		       (let*-typed ((ss (+ (store-size x) *default-sparse-store-increment*))
				    (δ-new (t/store-allocator index-store-vector ss) :type index-store-vector)
				    (sto-new (t/store-allocator ,(cl x) ss) :type ,(store-type (cl x))))
			 ,(code 'sto 'sto-new 'δg 'δ-new)
			 (setf (slot-value x 'neighbours) δ-new
			       (slot-value x 'store) sto-new)))))
	 (let-typed ((f (fence x) :type index-store-vector))
	   (loop :for i :from (1+ c) :below (length (fence x)) :do (incf (aref f i))))
	 (setf idx lb)))
     (setf
      (aref (δ-i x) (the index-type idx)) r
      (t/store-ref ,(cl x) (t/store ,(cl x) x) (the index-type idx)) (t/coerce ,(field-type (cl x)) value))))
;;
