;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved.
;;;
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;;
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:matlisp)

(deft/generic (t/blas-copy! #'subtypep) sym (x st-x y st-y))
(deft/method t/blas-copy! (sym blas-mixin) (x st-x y st-y)
  (let ((ncp? (null st-x)) (ftype (field-type sym)))
    (using-gensyms (decl (x y) (sto-x))
      `(let (,@decl)
	 (declare (type ,sym ,@(unless ncp? `(,x)) ,y)
		  ,@(when ncp? `((type ,(field-type sym) ,x))))
	 ,(recursive-append
	   (when ncp? `(with-field-element ,sym (,sto-x ,x)))
	   `(ffuncall ,(blas-func "copy" ftype)
		      (:& :int) (the index-type (total-size ,y))
		      (:* ,(lisp->mffi ftype) ,@(unless ncp? `(:+ (head ,x)))) ,(if ncp? sto-x `(t/store ,sym ,x))
		      (:& :int) (the index-type ,(if ncp? 0 st-x))
		      (:* ,(lisp->mffi ftype) :+ (head ,y)) (t/store ,sym ,y)
		      (:& :int) (the index-type ,st-y)))
	 ,y))))

;;
(deft/generic (t/copy! #'(lambda (a b) (strict-compare (list #'subtypep #'subtypep) a b))) (clx cly) (x y))
(deft/method t/copy! ((clx dense-tensor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (ref-x ref-y idx))
    `(let* (,@decl)
       (declare (type ,clx ,x)
		(type ,cly ,y))
       (very-quickly
	 (dorefs (,idx (dimensions ,y))
		 ((,ref-x ,x :type ,clx)
		  (,ref-y ,y :type ,cly))
	   (setf ,ref-y ,(if (and (subtypep (field-type clx) 'cl:real) (real-subtypep (field-type cly))) ;;Coercion messes up optimization in SBCL, so we specialize.
			     `(the ,(field-type cly) (complex (t/strict-coerce (,(field-type clx) ,(real-subtypep (field-type cly))) ,ref-x) (t/fid+ ,(real-subtypep (field-type cly)))))
			     (if (eql clx cly) ref-x `(t/strict-coerce (,(field-type clx) ,(field-type cly)) ,ref-x))))))
	   ,y)))

(deft/method t/copy! ((clx t) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (ref-y idx cx))
    `(let* (,@decl
	    (,cx (t/coerce ,(field-type cly) ,x)))
       (declare (type ,cly ,y)
		(type ,(field-type cly) ,cx))
       ;;This should be safe
       (very-quickly
	 (dorefs (,idx (dimensions ,y))
	   ((,ref-y ,y :type ,cly))
	   (setf ,ref-y ,cx)))
       ,y)))

;;
(deft/method (t/copy! #'(lambda (x) (hash-table-storep (first x)))) ((clx stride-accessor) (cly graph-accessor)) (x y)
  (using-gensyms (decl (x y) (rstd cstd rdat key value r c ii jj s? v vi vr vd i col-stop row))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (let ((,cstd (strides ,x 1))
	     (,rstd (strides ,x 0))
	     (,rdat (make-array (dimensions ,x (if (slot-value ,y 'transposep) 0 1)) :initial-element nil)))
	 (loop :for ,key :being :the :hash-keys :of (t/store ,clx ,x)
	    :using (hash-value ,value)
	    :do (letv* ((,c ,r (floor (the index-type ,key) ,cstd) :type index-type index-type)
			(,r ,s? (floor (the index-type ,r) ,rstd) :type index-type index-type)
			(,ii ,jj (if (slot-value ,y 'transposep) (values ,c ,r) (values ,r ,c)) :type index-type index-type))
		  (if (zerop ,s?)
		      (push (cons ,ii (t/strict-coerce (,(field-type clx) ,(field-type cly)) ,value)) (aref ,rdat ,jj))
		      (error "strides of the tensor are not canonical."))))
	 (when (< (store-size ,y) (total-size ,x))
	   (setf (slot-value ,y 'neighbors) (t/store-allocator index-store-vector (total-size ,x))
		 (slot-value ,y 'store) (t/store-allocator ,cly (total-size ,x))))
	 (let-typed ((,vi (fence ,y) :type index-store-vector)
		     (,vr (δ-i ,y) :type index-store-vector)
		     (,vd (t/store ,cly ,y) :type ,(store-type cly)))
	   (setf (aref ,vi 0) 0)
	   (very-quickly
	     (loop :for ,i :from 0 :below (length ,rdat)
		:with ,col-stop := 0
		:do (let ((,row (sort (aref ,rdat ,i) #'(lambda (x y) (< (the index-type x) (the index-type y))) :key #'car)))
		      (loop :for (,r . ,v) :in ,row
			 :do (locally
				 (declare (type ,(field-type cly) ,v)
					  (type index-type ,r))
			       (setf (aref ,vr ,col-stop) ,r)
			       (t/store-set ,cly ,v ,vd ,col-stop)
			       (incf ,col-stop)))
		      (setf (aref ,vi (1+ ,i)) ,col-stop)))))
	 ,y))))

(deft/method (t/copy! #'(lambda (x) (hash-table-storep (first x)))) ((clx stride-accessor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (rstd cstd key value r c s?))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (copy! (t/fid+ ,(field-type cly)) ,y)
       (let ((,cstd (strides ,x 1))
	     (,rstd (strides ,x 0)))
	 (loop :for ,key :being :the :hash-keys :of (t/store ,clx ,x)
	    :using (hash-value ,value)
	    :do (letv* ((,c ,r (floor (the index-type ,key) ,cstd) :type index-type index-type)
			(,r ,s? (floor (the index-type ,r) ,rstd) :type index-type index-type))
		  (if (zerop ,s?)
		      (setf (ref ,y ,r ,c) (t/strict-coerce (,(field-type clx) ,(field-type cly)) ,value))
		      (error "strides of the tensor are not canonical."))))
	 ,y))))

(deft/method (t/copy! #'(lambda (x) (hash-table-storep (second x)))) ((clx graph-accessor) (cly stride-accessor)) (x y)
  (using-gensyms (decl (x y) (key vi vr vd i j))
   `(let (,@decl)
      (declare (type ,clx ,x) (type ,cly ,y))
      (loop :for ,key :being :the :hash-keys :of (t/store ,cly ,y)
	 :do (remhash ,key (t/store ,cly ,y)))
      (let-typed ((,vi (fence ,x) :type index-store-vector)
		  (,vr (δ-i ,x) :type index-store-vector)
		  (,vd (t/store ,clx ,x) :type ,(store-type clx)))
	(if (slot-value ,x 'transposep)
	    (very-quickly
	      (loop :for ,j :from 0 :below (1- (length ,vi))
		 :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
			:do (setf (ref ,y ,j (aref ,vr ,i)) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (aref ,vd ,i))))))
	    (very-quickly
	      (loop :for ,j :from 0 :below (1- (length ,vi))
		 :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
			:do (setf (ref ,y (aref ,vr ,i) ,j) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (aref ,vd ,i))))))))
      ,y)))

(deft/method t/copy! ((clx graph-accessor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (vi vr vd i j))
   `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (copy! (t/fid+ ,(field-type cly)) ,y)
       (let-typed ((,vi (fence ,x) :type index-store-vector)
		   (,vr (δ-i ,x) :type index-store-vector)
		   (,vd (t/store ,clx ,x) :type ,(store-type clx)))
	 (if (slot-value ,x 'transposep)
	     (very-quickly
	       (loop :for ,j :from 0 :below (1- (length ,vi))
		  :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
			 :do (setf (ref ,y ,j (aref ,vr ,i)) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx ,vd ,i))))))
	     (very-quickly
	       (loop :for ,j :from 0 :below (1- (length ,vi))
		  :do (loop :for ,i :from (aref ,vi ,j) :below (aref ,vi (1+ ,j))
			 :do (setf (ref ,y (aref ,vr ,i) ,j) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx ,vd ,i))))))))
       ,y)))

#+nil
(deft/method t/copy! ((clx graph-tensor) (cly graph-tensor)) (x y)
  (using-gensyms (decl (x y) (idx i j m))
    (binding-gensyms (gm gf)
      (flet ((macro-expander (transpose-p)
	       `((loop :for ,(gm j) :of-type index-type :from 0 :below (1- (length (memoizing (fence ,x) :global t)))
		    :do (loop :for ,(gm m) :of-type index-type :from (aref (memoizing (fence ,x) :global t) ,j) :below (aref (memoizing (fence ,x) :global t) (1+ ,j))
			   :do (let-typed ((,i (aref (memoizing (δ-i ,x) :type index-store-vector :global t) ,m) :type index-type))
				 (setf ;;set hash
				  (aref ,idx 0) ,i (aref ,idx 1) ,j
				  (aref (memoizing (slot-value ,y 'stride-hash) :type index-store-vector :global t) ,m) (stride-hash ,idx (memoizing (strides ,y) :type index-store-vector :global t))
				  ;;set index
				  (aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 0) (aref ,idx ,(if transpose-p 1 0))
				  (aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 1) (aref ,idx ,(if transpose-p 0 1))
				  ;;Set value
				  (t/store-ref ,cly (memoizing (slot-value ,y 'store) :type ,(store-type cly) :global t) ,m) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx) :global t) ,i)))))))))
	`(let (,@decl)
	   (declare (type ,clx ,x) (type ,cly ,y))
	   (with-memoization ()
	     (let-typed ((,idx (t/store-allocator index-store-vector 2)))
	       (if (eql (slot-value ,x 'transposep) (slot-value ,x 'transposep))
		   (progn ,@(macro-expander t)) (progn ,@(macro-expander nil)))
	       (setf (slot-value ,y 'tail) (total-size ,x))))
	   ,y)))))

(deft/method t/copy! ((clx graph-tensor) (cly coordinate-tensor)) (x y)
  (using-gensyms (decl (x y) (idx i j m))
    (flet ((macro-expander (transpose-p)
	     `((loop :for ,j :of-type index-type :from 0 :below (1- (length (memoizing (fence ,x) :global t)))
		  :do (loop :for ,m :of-type index-type :from (aref (memoizing (fence ,x) :global t) ,j) :below (aref (memoizing (fence ,x) :global t) (1+ ,j))
			 :do (let-typed ((,i (aref (memoizing (δ-i ,x) :type index-store-vector :global t) ,m) :type index-type))
			       (setf ;;set hash
				(aref ,idx 0) ,i (aref ,idx 1) ,j
				(aref (memoizing (slot-value ,y 'stride-hash) :type index-store-vector :global t) ,m) (stride-hash ,idx (memoizing (strides ,y) :type index-store-vector :global t))
				;;set index
				(aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 0) (aref ,idx ,(if transpose-p 1 0))
				(aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 1) (aref ,idx ,(if transpose-p 0 1))
				;;Set value
				(t/store-ref ,cly (memoizing (slot-value ,y 'store) :type ,(store-type cly) :global t) ,m) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx) :global t) ,i)))))))))
      `(let (,@decl)
	 (declare (type ,clx ,x) (type ,cly ,y))
	 (with-memoization ()
	   (let-typed ((,idx (t/store-allocator index-store-vector 2)))
	     (if (slot-value ,x 'transposep) (very-quickly ,@(macro-expander t)) (very-quickly ,@(macro-expander nil)))
	     (setf (slot-value ,y 'tail) (total-size ,x))))
	 ,y))))

(deft/method t/copy! ((clx hash-tensor) (cly coordinate-tensor)) (x y)
  (using-gensyms (decl (x y) (idx ii k v))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (with-memoization ()
	 (let ((,idx (t/store-allocator index-store-vector (memoizing (total-size ,x) :global t))))
	   (iter (for (,k ,v) in-hashtable (store ,x))
		 (setf (aref ,idx ,ii) ,k)
		 (counting t into ,ii))
	   (lvec-copy (memoizing (total-size ,x) :global t) (sort ,idx #'<) 0 (memoizing (slot-value ,y 'stride-hash) :type index-store-vector :global t) 0))
	 (setf (slot-value ,y 'tail) (memoizing (total-size ,x) :global t))
	 ;;
	 (iter (for ,k in-vector (memoizing (slot-value ,y 'stride-hash) :global t) with-index ,ii below (memoizing (total-size ,x) :global t))
	       (lvec-copy (memoizing (order ,x) :global t)
			  (the index-store-vector (invert-hash (- ,k (memoizing (head ,x) :global t)) (memoizing (slot-value ,x 'stride-pivot) :global t) (memoizing (strides ,x) :global t) (memoizing (dimensions ,x) :global t))) 0
			  (memoizing (indices ,y) :type index-store-matrix :global t) (* (memoizing (order ,x) :global t) ,ii))
	       (setf (t/store-ref ,cly (memoizing (t/store ,cly ,y) :type ,(store-type cly) :global t) ,ii)
		     (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx) :global t) ,k)))))
       ,y)))

(deft/method t/copy! ((clx coordinate-tensor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (idx ii))
    `(let (,@decl)
       (declare (type ,clx ,x) (type ,cly ,y))
       (with-memoization ()
	 (let ((,idx (t/store-allocator index-store-vector (order ,x))))
	   (iter (for ,ii from 0 below (slot-value ,x 'tail))
		 (lvec-copy (memoizing (order ,x) :global t) (memoizing (indices ,x) :global t) (* ,ii (memoizing (order ,x) :global t)) ,idx 0 :key #'row-major-aref :lock #'(setf aref))
		 (setf (t/store-ref ,cly (memoizing (store ,y) :type ,(store-type cly) :global t) (+ (memoizing (head ,y) :global t) (stride-hash ,idx (memoizing (strides ,y) :global t))))
		       (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx (memoizing (store ,x) :type ,(store-type clx) :global t) ,ii))))))
       ,y)))

#+nil
(deft/method t/copy! ((clx coordinate-tensor) (cly graph-tensor)) (x y)
  (using-gensyms (decl (x y) (idx i j m))
    (binding-gensyms (gm gf)
      (flet ((macro-expander (transpose-p)
	       `((loop :for ,(gm m) :of-type index-type :from 0 :below (1- (memoizing (slot-value ,x 'tail) :type index-type :global t))
		    :do (loop :for ,m :of-type index-type :from (aref (memoizing (fence ,x) :global t) ,j) :below (aref (memoizing (fence ,x) :global t) (1+ ,j))
			   :do (let-typed ((,i (aref (memoizing (δ-i ,x) :type index-store-vector :global t) ,m) :type index-type))
				 (setf ;;set hash
				  (aref ,idx 0) ,i (aref ,idx 1) ,j
				  (aref (memoizing (slot-value ,y 'stride-hash) :type index-store-vector :global t) ,m) (stride-hash ,idx (memoizing (strides ,y) :type index-store-vector :global t))
				  ;;set index
				  (aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 0) (aref ,idx ,(if transpose-p 1 0))
				  (aref (memoizing (indices ,y) :type index-store-matrix :global t) ,m 1) (aref ,idx ,(if transpose-p 0 1))
				  ;;Set value
				  (t/store-ref ,cly (memoizing (slot-value ,y 'store) :type ,(store-type cly) :global t) ,m) (t/strict-coerce (,(field-type clx) ,(field-type cly)) (t/store-ref ,clx (memoizing (slot-value ,x 'store) :type ,(store-type clx) :global t) ,i)))))))))
	`(let (,@decl)
	   (declare (type ,clx ,x) (type ,cly ,y))
	   (with-memoization ()
	     (let-typed ((,idx (t/store-allocator index-store-vector 2)))
	       (if (slot-value ,x 'transposep) (very-quickly ,@(macro-expander t)) (very-quickly ,@(macro-expander nil)))
	       (setf (slot-value ,y 'tail) (total-size ,x))))
	   ,y)))))

#+nil
(define-tensor-method transpose ((g graph-accessor :x) &optional permutation)
  `(if (and permutation (= (permutation-size permutation) 1)) (copy g)
       (let ((adj (make-array (dimensions g (if (slot-value g 'transposep) 1 0)) :initial-element nil))
	     (ret (zeros (if (slot-value g 'transposep) (dimensions g) (reverse (dimensions g))) ',(cl :x) (store-size g))))
	 (when (slot-value g 'transposep) (setf (slot-value ret 'dimensions) (reverse (dimensions ret))
						(slot-value ret 'transposep) t))
	 (iter (for u from 0 below (1- (length (fence g))))
	       (letv* ((ll rr (fence g u)))
		 (iter (for v in-vector (δ-i g) from ll below rr with-index iuv) (push ,@(if (subtypep (cl :x) 'tensor) `((cons u iuv)) `(u)) (aref adj v)))))
	 (iter (for v from 0 below (1- (length (fence ret))))
	       (iter (for ,@(if (subtypep (cl :x) 'tensor) `((u . iuv)) `(u)) in (setf (aref adj v) (sort (aref adj v) #'< ,@(if (subtypep (cl :x) 'tensor) `(:key #'car)))))
		     (let ((idx (+ (fence ret v) j)))
		       (setf (aref (δ-i ret) idx) u
			     ,@(if (subtypep (cl :x) 'tensor) `((t/store-ref ,(cl :x) (t/store ,(cl :x) ret) idx) (t/store-ref ,(cl :x) (t/store ,(cl :x) g) iuv))))
		       (counting t into j) (finally (setf (aref (fence ret) (1+ v)) (+ (fence ret v) j))))))
	 ret)))
;;
#+nil
(let ((a (zeros '(10 10) '((complex double-float) stride-accessor hash-table)))
      (b (zeros '(10 10) '((complex double-float) graph-accessor) 100))
      (x (zeros '(10 10) '((complex double-float)))))
  (iter (repeat 10)
	(letv* ((i j (values (random 5) (random 5))))
	  (setf (ref a i j) (complex (random 1d0) (random 1d0))
		(ref x i j) (ref a i j))))
					;(t/copy! (#.(tensor '(complex double-float) 'stride-accessor 'hash-table) #.(tensor '(complex double-float) 'graph-accessor)) a b)
  (norm (t- (copy (copy! a b) '((complex double-float)))
	    (copy a '((complex double-float)))))
  ;;(norm (t- (copy b '((complex double-float))) x))
  ;;(copy! a (zeros (dimensions a) '((complex double-float))))
  ;;(norm (t- (copy a '((complex double-float))) (copy b '((complex double-float)))))
  )

;;
(closer-mop:defmethod copy! :before ((x tensor) (y tensor))
  (assert (and (very-quickly (lvec-eq (dimensions x) (dimensions y) '=))) nil 'tensor-dimension-mismatch)
  (assert (<= (total-size x) (store-size y)) nil 'tensor-insufficient-store))

(define-tensor-method copy! ((x array) (y dense-tensor :y t))
  `(let-typed ((sto-y (store y) :type ,(store-type (cl :y))))
     (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))
									     (of-x (make-stride-rmj (coerce (array-dimensions x) 'index-store-vector)))))))
	   (setf (t/store-ref ,(cl :y) sto-y of-y) (t/coerce ,(field-type (cl :y)) (row-major-aref x of-x))))
     y))

(define-tensor-method copy! ((x dense-tensor :x t) (y array))
  `(let-typed ((sto-x (store x) :type ,(store-type (cl :x))))
     (iter (for-mod idx from 0 below (dimensions x) with-iterator ((:stride ((of-x (strides x) (head x))
									     (of-y (make-stride-rmj (coerce (array-dimensions y) 'index-store-vector)))))))
	   (setf (row-major-aref y of-y) (t/store-ref ,(cl :x) sto-x of-x)))
     y))

#+nil
(closer-mop:defmethod copy! :before ((a base-tensor) (b compressed-sparse-matrix))
  (assert (<= (store-size a) (store-size b)) nil 'tensor-insufficient-store))

(define-tensor-method copy! ((x tensor :x) (y tensor :y t))
  (recursive-append
   (when (and (eql (cl :x) (cl :y)) (subtypep (cl :y) 'external-mixin))
     `(if-let ((strd (and (call-fortran? y (t/blas-lb ,(cl :y) 1)) (blas-copyablep x y))))
	(t/blas-copy! ,(cl :y) x (first strd) y (second strd))))
   `(t/copy! (,(cl :x) ,(cl :y)) x y))
  'y)

(define-tensor-method copy! ((x t) (y dense-tensor :y t))
  (recursive-append
   (when (subtypep (cl :y) 'external-mixin)
     `(if-let ((strd (and (call-fortran? y (t/blas-lb ,(cl :y) 1)) (consecutive-storep y))))
	(t/blas-copy! ,(cl :y) (t/coerce ,(field-type (cl :y)) x) nil y strd)))
   `(t/copy! (t ,(cl :y)) x y)))

(define-tensor-method copy! ((x t) (y coordinate-tensor :y t))
  `(with-memoization ()
     (loop :for i :from 0 :below (slot-value y 'tail)
	:do (setf (t/store-ref ,(cl :y) (memoizing (store y) :type ,(store-type (cl :y)) :global t) i)
		  (memoizing (t/coerce ,(field-type (cl :y)) x) :type ,(field-type (cl :y)) :global t)))
     y))

;;Generic function defined in src;base;generic-copy.lisp
(closer-mop:defmethod copy! ((tensor dense-tensor) (type symbol))
  (cond
    ((eql type 'array) (copy! tensor (make-array (lvec->list (dimensions tensor)))))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (order arr)) (apply #'ref arr idx)
		      (loop :for i :from 0 :below (aref (dimensions arr) n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree tensor nil)))
    ((or (null type) (subtypep type 'dense-tensor))
     (copy! tensor (zeros (dimensions tensor) (or type (type-of tensor)))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

(closer-mop:defmethod copy! ((from foreign-dense-tensor) (to (eql nil)))
  (copy! from (tensor (field-type (class-of from)) 'simple-dense-tensor)))

(closer-mop:defmethod copy! ((tensor tensor) (type symbol))
  (cond
    ((or (null type) (subtypep type 'tensor))
     (let ((type (or type (type-of tensor))))
       (copy! tensor (zeros (dimensions tensor) type (if (subtypep type 'sparse-tensor) (total-size tensor))))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

#+nil
(closer-mop:defmethod copy-generic ((tensor sparse-tensor) type)
  (cond
    ((or (not type) (subtypep type 'sparse-tensor))
     (let ((ret (zeros (dimensions tensor) (or type (class-of tensor)) (store-size tensor))))
       (copy! tensor ret)))
    ((subtypep type 'standard-tensor)
     (let ((ret (zeros (dimensions tensor) type (store-size tensor))))
       (copy! tensor ret)))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))


;;
(closer-mop:defgeneric tricopy! (a b uplo?)
  (:documentation "Copy upper order, lower order, or diagonal.")
  (:generic-function-class tensor-method-generator))

(define-tensor-method tricopy! ((a dense-tensor :x) (b dense-tensor :x t) uplo?)
  `(ecase uplo?
     ,@(iter (for op in '(:u :uo :l :lo))
	     (collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
				    ((refa a :type ,(cl :x))
				     (refb b :type ,(cl :x)))
				    (setf refb refa)))))
     (:d
      (let-typed ((ss.a (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)) :type index-type)
		  (ss.b (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
		  (sto.a (store a) :type ,(store-type (cl :x)))
		  (sto.b (store b) :type ,(store-type (cl :x))))
	(loop :repeat (the index-type (lvec-min (dimensions b)))
	   :for of.a :of-type index-type := (head a) :then (the index-type (+ of.a ss.a))
	   :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
	   :do (setf (t/store-ref ,(cl :x) sto.b of.b) (t/store-ref ,(cl :x) sto.a of.a))))))
  'b)

(define-tensor-method tricopy! ((a t) (b dense-tensor :x) uplo?)
  `(let ((a (t/coerce ,(field-type (cl :x)) a)))
     (ecase uplo?
       ,@(iter (for op in '(:u :uo :l :lo))
	       (collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
				((refb b :type ,(cl :x)))
				(setf refb a)))))
       (:d
	(let-typed ((ss.b (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
		    (sto.b (store b) :type ,(store-type (cl :x))))
	  (loop :repeat (the index-type (lvec-min (dimensions b)))
	     :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
	     :do (setf (t/store-ref ,(cl :x) sto.b of.b) a)))))
     b))
;;
(deft/generic (t/blas-swap! #'subtypep) sym (x st-x y st-y))
(deft/method t/blas-swap! (sym blas-mixin) (x st-x y st-y)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (x y))
      `(let (,@decl)
	 (declare (type ,sym ,x ,y))
	 (ffuncall ,(blas-func "swap" ftype)
		   (:& :int) (total-size ,y)
		   (:* ,(lisp->mffi ftype) :+ (head ,x)) (the ,(store-type sym) (store ,x)) (:& :int) ,st-x
		   (:* ,(lisp->mffi ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :int) ,st-y)
	 ,y))))

(deft/generic (t/swap! #'subtypep) sym (x y))
(deft/method t/swap! (sym dense-tensor) (x y)
  (using-gensyms (decl (x y) (idx ref-x ref-y))
    `(let* (,@decl)
       (declare (type ,sym ,x ,y))
       (very-quickly
	 (dorefs (,idx (dimensions ,x))
		 ((,ref-x ,x :type ,sym)
		  (,ref-y ,y :type ,sym))
	   (rotatef ,ref-x ,ref-y))
	 ,y))))
;;---------------------------------------------------------------;;
(closer-mop:defmethod swap! :before ((x dense-tensor) (y dense-tensor))
  (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
	  'tensor-dimension-mismatch))

(define-tensor-method swap! ((x dense-tensor :x t) (y dense-tensor :x t))
  (recursive-append
   (when (subtypep (cl :x) 'external-mixin)
     `(if-let ((strd (and (call-fortran? x (t/blas-lb ,(cl :x) 1)) (blas-copyablep x y))))
	(t/blas-swap! ,(cl :x) x (first strd) y (second strd)))))
  `(t/swap! ,(cl :x) x y))
