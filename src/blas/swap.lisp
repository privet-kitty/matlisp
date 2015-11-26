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

;;
(deft/generic (t/blas-swap! #'subtypep) sym (x st-x y st-y))
(deft/method t/blas-swap! (sym blas-mixin) (x st-x y st-y)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (x y))
      `(let (,@decl)
	 (declare (type ,sym ,x ,y))
	 (ffuncall ,(blas-func "swap" ftype)
		   (:& :integer) (total-size ,y)
		   (:* ,(lisp->ffc ftype) :+ (head ,x)) (the ,(store-type sym) (store ,x)) (:& :integer) ,st-x
		   (:* ,(lisp->ffc ftype) :+ (head ,y)) (the ,(store-type sym) (store ,y)) (:& :integer) ,st-y)
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
(defmethod swap! :before ((x dense-tensor) (y dense-tensor))
  (assert (very-quickly (lvec-eq (the index-store-vector (dimensions x)) (the index-store-vector (dimensions y)) #'=)) nil
	  'tensor-dimension-mismatch))

(define-tensor-method swap! ((x dense-tensor :x t) (y dense-tensor :x t))
  (recursive-append
   (when (subtypep (cl :x) 'blas-mixin)
     `(if-let (strd (and (call-fortran? x (t/blas-lb ,(cl :x) 1)) (blas-copyablep x y)))
	(t/blas-swap! ,(cl :x) x (first strd) y (second strd)))))
  `(t/swap! ,(cl :x) x y))
