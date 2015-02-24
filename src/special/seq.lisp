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

(defun realtype-max (lst)
  (let ((x (first (sort lst
			#'(lambda (x y)
			    (cart-typecase (x y)
			      ((integer integer) (> (integer-length x) (integer-length y)))
			      ((ratio integer) t)
			      ((float t) t)
			      ((float float) (> (float-digits x) (float-digits y)))))))))
    (etypecase x
      ((or integer float) (type-of x))
      (ratio 'rational))))

(defun range (start end &key (h 1) list-outputp)
  (declare (type real start end h))
  (let ((quo (ceiling (if (> start end) (- start end) (- end start)) h))
	(h (if (> start end) (- h) h)))
    (if (= quo 0) nil
	(if (not list-outputp)
	    (let* ((type (realtype-max (list h start end (+ h start) (- end h)))))
	      (mapsor! (let ((ori (coerce start type)) (h (coerce h type)))
			 (lambda (idx y) (declare (ignore idx y)) (prog1 ori (incf ori h))))
		       nil (zeros quo (list type))))
	    (loop :for i :from 0 :below quo
	       :for ori := start :then (+ ori h)
	       :collect ori)))))

(defun linspace (start end &key (num-points (1+ (abs (- start end)))) list-outputp)
  (let* ((num-points (floor num-points))
	 (h (/ (- end start) (1- num-points))))
    (range start (+ h end) :h (abs h) :list-outputp list-outputp)))
