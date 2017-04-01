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

;;TODO: make this generic.
(definline transpose! (A &optional permutation)
  "
  Syntax
  ======
  (t:TRANSPOSE! a [permutation])

  Purpose
  =======
  Exchange the arguments of the tensor in place. The default
  is to swap the first and last arguments of the tensor.

  Settable
  ========
  (setf (TRANSPOSE! tensor permutation) value)

  is basically the same as
  (copy! value (TRANSPOSE! tensor permutation)).

  NOTE: This will have side-effects even if copy! doesn't succeed."
  (declare (type dense-tensor a))
  (if permutation
      (progn
	(permute! (strides A) permutation)
	(permute! (dimensions A) permutation))
      (let-typed ((dims (dimensions A) :type index-store-vector)
		  (strd (strides A) :type index-store-vector))
	(rotatef (aref dims (1- (order A))) (aref dims 0))
	(rotatef (aref strd (1- (order A))) (aref strd 0))))
  (setf (slot-value A 'memos) nil)
  A)

(definline (setf transpose!) (value A &optional permutation)
  (copy! value (transpose! A permutation)))

(definline transpose~ (A &optional permutation)
  "
  Syntax
  ======
  (TRANSPOSE~ a permutation)

  Purpose
  =======
  Like TRANSPOSE!, but the permuted strides and dimensions are part of
  a new tensor object instead, the store being shared with the given
  tensor.

  Settable
  ========
  (setf (TRANSPOSE~ tensor permutation) value)

  is basically the same as
  (copy! value (TRANSPOSE~ tensor permutation))"
  (declare (type dense-tensor A))
  (transpose! (subtensor~ A nil) permutation))

(definline (setf transpose~) (value A &optional permutation)
  (declare (type dense-tensor A))
  (copy! value (transpose~ A permutation)))

(closer-mop:defgeneric transpose (A &optional permutation)
  (:documentation "
  Syntax
  ======
  (TRANSPOSE~ a permutation)

  Purpose
  =======
  Like TRANSPOSE!, but the permutation is applied on a copy of
  the given tensor.

  Settable
  ========
  (setf (TRANSPOSE tensor permutation) value)

  is the same as (setf (transpose~ ..) ..)")
  (:method :before ((A base-accessor) &optional permutation)
     (assert (or (not permutation) (< (permutation-size permutation) (order A))) nil 'tensor-index-rank-mismatch))
  (:method ((A dense-tensor) &optional permutation)
    (copy (transpose~ A permutation)))
  (:generic-function-class tensor-method-generator))

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

(definline (setf transpose) (value A &optional permutation)
  (declare (type dense-tensor A))
  (copy! value (transpose~ A permutation))
  A)

;;This is a bit more complicated, now that we are no longer in S_2
;;Computing the inverse permutation is trivial in the cyclic representation,
;;but probably not worth the trouble for this ugly macro.
#+nil
(defmacro with-transpose! (matlst &rest body)
  `(progn
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)
     ,@body
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)))
;;
(definline t:conjugate! (A)
  "
  Syntax
  ======
  (t:conjugate! A)

  Purpose
  =======
  Destructively modifies A into its complex conjugate (not hermitian conjugate).

  (t:imagpart~ A) <- (- (t:imagpart~ A)) "
  (etypecase A
    (cl:number (cl:conjugate A))
    (dense-tensor (if (eql (realified-tensor (class-of A)) (type-of A)) A
		      (progn (scal! -1 (t:imagpart~ A)) A)))))

(definline t:conjugate (A)
  "
  Syntax
  ======
  (t:conjugate A)

  Purpose
  =======
  Like conjugate!, but non-destructive."
  (typecase A
    (cl:number (cl:conjugate A))
    (t (t:conjugate! (copy A)))))

;;
(definline ctranspose! (A &optional permutation)
  "
   Syntax
   ======
   (CTRANSPOSE! A [permutation])

   Purpose
   =======
   Hermitian transpose of A (destructive).
"
  (typecase A
    (cl:number (cl:conjugate A))
    (t (t:conjugate! (transpose! A permutation)))))

(definline ctranspose (A &optional permutation)
  "
  Syntax
  ======
  (CTRANSPOSE A [permutation])

  Purpose
  =======
  Like CTRANSPOSE!, but non-destructive."
  (typecase A
    (cl:number (cl:conjugate A))
    (t (ctranspose! (copy A) permutation))))
