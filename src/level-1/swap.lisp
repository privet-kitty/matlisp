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

(defmacro generate-typed-swap! (func (tensor-class blas-func))
  ;;Be very careful when using functions generated by this macro.
  ;;Indexes can be tricky and this has no safety net
  ;;Use only after checking the arguments for compatibility.
  (let* ((opt (get-tensor-class-optimization tensor-class)))
    (assert opt nil 'tensor-cannot-find-optimization :tensor-class tensor-class)
    `(defun ,func (x y)
       (declare (type ,tensor-class x y))
       (if-let (strd-p (blas-copyable-p x y))
	 (,blas-func (number-of-elements x) (store x) (first strd-p) (store y) (second strd-p) (head x) (head y))
	 (let ((f-sto (store x))
	       (t-sto (store y)))
	   (declare (type ,(linear-array-type (getf opt :store-type)) f-sto t-sto))
	   (very-quickly
	     ;;One would question the wisdom in calling the Fortran method here.
	     ;;Simple benchmarks proved that SBCL is as quick as or better than
	     ;;OpenBLAS's methods
	     (mod-dotimes (idx (dimensions x))
	       with (linear-sums
		     (f-of (strides x) (head x))
		     (t-of (strides y) (head y)))
	       do ,(funcall (getf opt :swapper) 'f-sto 'f-of 't-sto 't-of)))))
       y)))

(generate-typed-swap! real-typed-swap! (real-tensor dswap))
(generate-typed-swap! complex-typed-swap! (complex-tensor zswap))
;;---------------------------------------------------------------;;

(defgeneric swap! (x y)
  (:documentation
"
  Sytnax
  ======
  (SWAP! x y)

  Purpose
  =======
  Given tensors X,Y, performs:

              X <-> Y

  and returns Y.

  X, Y must have the same dimensions.
")
  (:method :before ((x standard-tensor) (y standard-tensor))
	   (unless (idx= (dimensions x) (dimensions y))
	     (error 'tensor-dimension-mismatch)))
  (:method ((x complex-tensor) (y real-tensor))
    (error 'coercion-error :from 'complex-tensor :to 'real-tensor))
  (:method ((x real-tensor) (y complex-tensor))
    (error 'coercion-error :from 'complex-tensor :to 'real-tensor)))

(defmethod swap! ((x real-tensor) (y real-tensor))
  (real-typed-swap! x y))

(defmethod swap! ((x complex-tensor) (y complex-tensor))
  (complex-typed-swap! x y))