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
  (TRANSPOSE! a [permutation])

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

(definline transpose (A &optional permutation)
  "
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

  is the same as (setf (transpose~ ..) ..)"
  (declare (type dense-tensor A))
  (copy (transpose~ A permutation)))

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
(definline conjugate! (A)
  "
  Syntax
  ======
  (conjugate! A)

  Purpose
  =======
  Destructively modifies A into its complex conjugate (not hermitian conjugate).

  (tensor-imagpart~ A) <- (- (tensor-imagpart~ A)) "
  (etypecase A
    (cl:number (cl:conjugate A))
    (dense-tensor (if (eql (realified-type A) (type-of A)) A
		      (progn (scal! -1 (imagpart~ A)) A)))))

(definline conjugate (A)
  "
  Syntax
  ======
  (conjugate A)

  Purpose
  =======
  Like conjugate!, but non-destructive."
  (typecase A
    (cl:number (cl:conjugate A))
    (t (conjugate! (copy A)))))

;;
(definline hconjugate! (A &optional permutation)
  "
   Syntax
   ======
   (HCONJUGATE! A [permutation])

   Purpose
   =======
   Hermitian transpose of A (destructive).
"
  (declare (type dense-tensor A))
  (conjugate! (transpose! A permutation)))

(definline hconjugate (A &optional permutation)
  "
  Syntax
  ======
  (HCONJUGATE A [permutation])

  Purpose
  =======
  Like HCONJUGATE!, but non-destructive."
  (declare (type dense-tensor A))
  (hconjugate! (copy A) permutation))
