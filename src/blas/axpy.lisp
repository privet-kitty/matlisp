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

(deft/generic (t/blas-axpy! #'subtypep) sym (a x st-x y st-y))
(deft/method t/blas-axpy! (sym blas-mixin) (a x st-x y st-y)
  (let ((apy? (null x)) (ftype (field-type sym)))
    (using-gensyms (decl (a x y) (sto-x))
      `(let (,@decl)
	 (declare (type ,sym ,@(unless apy? `(,x)) ,y)
		  ,@(when apy? `((ignore ,x))))
	 ,(recursive-append
	   (when apy? `(with-field-element ,sym (,sto-x (t/fid* ,ftype))))
	   `(ffuncall ,(blas-func "axpy" ftype)
	      (:& :int) (the index-type (total-size ,y))
	      (:& ,(lisp->mffi ftype)) (the ,(field-type sym) ,a)
	      (:* ,(lisp->mffi ftype) ,@(unless apy? `(:+ (head ,x)))) ,(if apy? sto-x `(t/store ,sym ,x))
	      (:& :int) (the index-type ,(if apy? 0 st-x))
	      (:* ,(lisp->mffi ftype) :+ (head ,y)) (t/store ,sym ,y)
	      (:& :int) (the index-type ,st-y)))
	 ,y))))

(deft/generic (t/axpy! #'subtypep) sym (a x y))
(deft/method t/axpy! (sym dense-tensor) (a x y)
  (let ((apy? (null x)))
    (using-gensyms (decl (a x y) (idx ref-x ref-y))
      `(let (,@decl)
	 (declare (type ,sym ,@(unless apy? `(,x)) ,y)
		  (type ,(field-type sym) ,a)
		  ,@(when apy? `((ignore ,x))))
	 (very-quickly
	   (dorefs (,idx (dimensions ,y))
		   (,@(unless apy? `((,ref-x ,x :type ,sym)))
		      (,ref-y ,y :type ,sym))
		   (setf ,ref-y (t/f+ ,(field-type sym) ,@(if apy? `(,a) `((t/f* ,(field-type sym) ,a ,ref-x))) ,ref-y))))
	 ,y))))
;;---------------------------------------------------------------;;
(closer-mop:defgeneric axpy! (alpha x y)
  (:documentation
   " 
 Syntax
 ======
 (AXPY! alpha x y)

 Y <- alpha * x + y

 If x is T, then

 Y <- alpha + y

 Purpose
 =======
  Same as AXPY except that the result
  is stored in Y and Y is returned.
")
  (:method :before ((alpha number) (x base-tensor) (y base-tensor))
	   (assert (lvec-eq (dimensions x) (dimensions y) #'=) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(define-tensor-method axpy! (alpha (x dense-tensor :y) (y dense-tensor :y t))
  `(let ((alpha (t/coerce ,(field-type (cl :y)) alpha)))
     (declare (type ,(field-type (cl :y)) alpha))
     ,(recursive-append
       (when (subtypep (cl :y) 'blas-mixin)
	 `(if-let ((strd (and (call-fortran? y (t/blas-lb ,(cl :y) 1)) (blas-copyablep x y))))
	    (t/blas-axpy! ,(cl :y) alpha x (first strd) y (second strd))))
       `(t/axpy! ,(cl :y) alpha x y))
     y))

(define-tensor-method axpy! (alpha x (y dense-tensor :y t))
  `(let ((alpha (t/coerce ,(field-type (cl :y)) alpha)))
     (declare (type ,(field-type (cl :y)) alpha))
     (when x (setq alpha (t/f* ,(field-type (cl :y)) alpha (t/coerce ,(field-type (cl :y)) x))))
     (unless (t/f= ,(field-type (cl :y)) alpha (t/fid+ ,(field-type (cl :y))))
       ,(recursive-append
	 (when (subtypep (cl :y) 'blas-mixin)
	   `(if-let ((strd (and (call-fortran? y (t/blas-lb ,(cl :y) 1)) (consecutive-storep y))))
	      (t/blas-axpy! ,(cl :y) alpha nil nil y strd)))
	 `(t/axpy! ,(cl :y) alpha nil y)))
     y))
;;
(defgeneric axpy (alpha x y)
  (:documentation
   "
 Syntax
 ======
 (AXPY alpha x y)

 Purpose
 =======
 Computes  
      
                 ALPHA * X + Y

 where ALPHA is a scalar and X,Y are
 tensors.

 The result is stored in a new matrix 
 that has the same dimensions as Y.

 X,Y must have the same dimensions.
")
  (:method (alpha x (y dense-tensor))
    (axpy! alpha x (copy y (when (or (complexp alpha) (complexp x) (clinear-storep (type-of x)))
			     (complexified-tensor (type-of y)))))))

