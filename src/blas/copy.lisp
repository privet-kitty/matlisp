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
(deft/method (t/blas-copy! #'blas-tensor-typep) (sym dense-tensor) (x st-x y st-y)
  (let ((ncp? (null st-x)) (ftype (field-type sym)))
    (using-gensyms (decl (x y) (sto-x))
      `(let (,@decl)
	 (declare (type ,sym ,@(unless ncp? `(,x)) ,y)
		  ,@(when ncp? `((type ,(field-type sym) ,x))))
	 ,(recursive-append
	   (when ncp? `(with-field-element ,sym (,sto-x ,x)))
	   `(ffuncall ,(blas-func "copy" ftype)
	      (:& :integer) (the index-type (total-size ,y))
	      (:* ,(lisp->ffc ftype) ,@(unless ncp? `(:+ (head ,x)))) ,(if ncp? sto-x `(t/store ,sym ,x))
	      (:& :integer) (the index-type ,(if ncp? 0 st-x))
	      (:* ,(lisp->ffc ftype) :+ (head ,y)) (t/store ,sym ,y)
	      (:& :integer) (the index-type ,st-y)))
	 ,y))))

;;
(deft/generic (t/copy! #'(lambda (a b) (strict-compare (list #'subtypep #'subtypep) a b))) (clx cly) (x y))
(deft/method t/copy! ((clx dense-tensor) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (sto-x sto-y of-x of-y idx))
    `(let* (,@decl
	    (,sto-x (t/store ,clx ,x))
	    (,sto-y (t/store ,cly ,y)))
       (declare (type ,clx ,x)
		(type ,cly ,y)
		(type ,(store-type clx) ,sto-x)
		(type ,(store-type cly) ,sto-y))
       (very-quickly
	 (iter (for-mod ,idx from 0 below (dimensions ,x) with-strides ((,of-x (strides ,x) (head ,x))
									(,of-y (strides ,y) (head ,y))))
	       (t/store-set ,cly
			    ;;Coercion messes up optimization in SBCL, so we specialize.
			    ,(if (and (subtypep (field-type clx) 'cl:real) (real-subtype (field-type cly)))
				 `(the ,(field-type cly) (complex (t/strict-coerce (,(field-type clx) ,(real-subtype (field-type cly)))
										   (t/store-ref ,clx ,sto-x ,of-x))
								  (t/fid+ ,(real-subtype (field-type cly)))))
				 (recursive-append
				  (unless (eql clx cly) `(t/strict-coerce (,(field-type clx) ,(field-type cly))))
				  `(t/store-ref ,clx ,sto-x ,of-x)))
			    ,sto-y ,of-y)))
	   ,y)))

(deft/method t/copy! ((clx t) (cly dense-tensor)) (x y)
  (using-gensyms (decl (x y) (sto-y of-y idx cx))
    `(let* (,@decl
	    (,sto-y (t/store ,cly ,y))
	    (,cx (t/coerce ,(field-type cly) ,x)))
       (declare (type ,cly ,y)
		(type ,(field-type cly) ,cx)
		(type ,(store-type cly) ,sto-y))
       ;;This should be safe
       (very-quickly
	 (iter (for-mod ,idx from 0 below (dimensions ,y) with-strides ((,of-y (strides ,y) (head ,y))))
	       (t/store-set ,cly ,cx ,sto-y ,of-y)))
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
	   (setf (slot-value ,y 'neighbours) (t/store-allocator index-store-vector (total-size ,x))
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
(defmethod copy! :before ((x tensor) (y tensor))
  (assert (very-quickly (lvec-eq (dimensions x) (dimensions y) '=)) nil 'tensor-dimension-mismatch))

(define-tensor-method copy! ((x array) (y tensor :y t))
  `(let-typed ((sto-y (store y) :type ,(store-type (cl y)))
	       (lst (make-list (array-rank x)) :type cons))
     (iter (for-mod idx from 0 below (dimensions y) with-strides ((of-y (strides y) (head y))))
	   (t/store-set ,(cl y) (t/coerce ,(field-type (cl y)) (apply #'aref x (lvec->list! idx lst))) sto-y of-y))
     y))

(define-tensor-method copy! ((x tensor :x t) (y array))
  `(let-typed ((sto-x (store x) :type ,(store-type (cl x)))
	       (lst (make-list (array-rank y)) :type cons))
     (iter (for-mod idx from 0 below (dimensions x) with-strides ((of-x (strides x) (head x))))
	   (setf (apply #'aref y (lvec->list! idx lst)) (t/store-ref ,(cl x) sto-x of-x)))
     y))

#+nil
(defmethod copy! :before ((a base-tensor) (b compressed-sparse-matrix))
  (assert (<= (store-size a) (store-size b)) nil 'tensor-insufficient-store))

(define-tensor-method copy! ((x tensor :x) (y tensor :y t))
  (recursive-append
   (when (and (eql (cl x) (cl y)) (blas-tensor-typep (cl y)))
     `(if-let (strd (and (call-fortran? y (t/blas-lb ,(cl y) 1)) (blas-copyablep x y)))
	(t/blas-copy! ,(cl y) x (first strd) y (second strd))))
   `(t/copy! (,(cl x) ,(cl y)) x y))
  'y)

(define-tensor-method copy! ((x t) (y dense-tensor :y t))
  (recursive-append
   (when (blas-tensor-typep (cl y))
     `(if-let (strd (and (call-fortran? y (t/blas-lb ,(cl y) 1)) (consecutive-storep y)))
	(t/blas-copy! ,(cl y) (t/coerce ,(field-type (cl y)) x) nil y strd)))
   `(t/copy! (t ,(cl y)) x y)))
;;
(defgeneric tricopy! (a b uplo?)
  (:documentation "Copy upper order, lower order, or diagonal."))

(define-tensor-method tricopy! ((a dense-tensor :x) (b dense-tensor :x t) uplo?)
  `(ecase uplo?
     ,@(iter (for op in '(:u :uo :l :lo))
	     (collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
				    ((refa a :type ,(cl a))
				     (refb b :type ,(cl b)))
				    (setf refb refa)))))
     (:d
      (let-typed ((ss.a (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides a)) :type index-type)
		  (ss.b (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
		  (sto.a (store a) :type ,(store-type (cl b)))
		  (sto.b (store b) :type ,(store-type (cl b))))
	(loop :repeat (the index-type (lvec-min (dimensions b)))
	   :for of.a :of-type index-type := (head a) :then (the index-type (+ of.a ss.a))
	   :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
	   :do (setf (t/store-ref ,(cl b) sto.b of.b) (t/store-ref ,(cl a) sto.a of.a))))))
  'b)

(define-tensor-method tricopy! ((a t) (b dense-tensor :x) uplo?)
  `(let ((a (t/coerce ,(field-type (cl b)) a)))
     (ecase uplo?
       ,@(iter (for op in '(:u :uo :l :lo))
	       (collect `(,op (dorefs (idx (dimensions b) :uplo? ,op)
				((refb b :type ,(cl b)))
				(setf refb a)))))
       (:d
	(let-typed ((ss.b (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (+ x y))) (strides b)) :type index-type)
		    (sto.b (store b) :type ,(store-type (cl b))))
	  (loop :repeat (the index-type (lvec-min (dimensions b)))
	     :for of.b :of-type index-type := (head b) :then (the index-type (+ of.b ss.b))
	     :do (setf (t/store-ref ,(cl b) sto.b of.b) a)))))
     b))

;;Generic function defined in src;base;generic-copy.lisp
(defmethod copy-generic ((tensor dense-tensor) type)
  (cond
    ((eql type 'array)
     (let ((ret (make-array (lvec->list (dimensions tensor)))))
       (copy! tensor ret)))
    ((member type '(list cons))
     (labels ((mtree (arr idx)
		(let ((n (length idx)))
		  (if (= n (order arr)) (apply #'ref arr idx)
		      (loop :for i :from 0 :below (aref (dimensions arr) n)
			 :collect (mtree arr (append idx (list i))))))))
       (mtree tensor nil)))
    ((or (not type) (consp type) (subtypep type 'dense-tensor))
     (copy! tensor (zeros (dimensions tensor) (cond ((null type) (type-of tensor))
						    ((symbolp type) type)
						    (t (apply #'tensor type))))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

(defmethod copy-generic ((tensor tensor) type)
  (cond
    ((or (not type) (consp type) (subtypep type 'tensor))
     (copy! tensor (zeros (dimensions tensor) (cond ((null type) (type-of tensor))
						    ((symbolp type) type)
						    (t (apply #'tensor type))))))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))

#+nil
(defmethod copy-generic ((tensor sparse-tensor) type)
  (cond
    ((or (not type) (subtypep type 'sparse-tensor))
     (let ((ret (zeros (dimensions tensor) (or type (class-of tensor)) (store-size tensor))))
       (copy! tensor ret)))
    ((subtypep type 'standard-tensor)
     (let ((ret (zeros (dimensions tensor) type (store-size tensor))))
       (copy! tensor ret)))
    (t (error "don't know how to copy ~a into ~a." (class-name (class-of tensor)) type))))
