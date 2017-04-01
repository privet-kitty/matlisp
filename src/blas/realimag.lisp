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

(definline t:realpart~ (tensor)
  "
  Syntax
  ======
  (t:realpart~ tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:realpart tensor))
    (dense-tensor (if (eql (realified-tensor (type-of tensor)) (type-of tensor)) tensor
		      (with-no-init-checks
			  (make-instance (realified-tensor (type-of tensor))
					 :parent tensor :store (store tensor)
					 :dimensions (dimensions tensor)
					 :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (the index-store-vector (strides tensor)))
					 :head (the index-type (* 2 (head tensor)))))))))

(definline t:imagpart~ (tensor)
  "
  Syntax
  ======
  (t:imagpart~ tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to the imaginary part of the TENSOR, if
  it is complex valued, otherwise returns NIL.

  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its imaginary part.
"
  (etypecase tensor
    (number (cl:imagpart tensor))
    (dense-tensor (if (eql (realified-tensor (type-of tensor)) (type-of tensor)) nil
		      (with-no-init-checks
			  (make-instance (realified-tensor (type-of tensor))
					 :parent tensor :store (store tensor)
					 :dimensions (dimensions tensor)
					 :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (the index-store-vector (strides tensor)))
					 :head (1+ (the index-type (* 2 (head tensor))))))))))

(definline t:realpart (tensor)
  "
  Syntax
  ======
  (t:realpart tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:realpart tensor))
    (dense-tensor (copy (t:realpart~ tensor)))))

(definline t:imagpart (tensor)
  "
  Syntax
  ======
  (t:imagpart tensor)
 
  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:imagpart tensor))
    (dense-tensor (if-let ((ip (t:imagpart~ tensor)))
		    (copy ip)
		    (zeros (dimensions tensor) (tensor (field-type (type-of tensor))))))))
