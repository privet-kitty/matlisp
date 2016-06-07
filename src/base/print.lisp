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
;; Routines for printing a tensors/matrices nicely.

(defparameter *tensor-print-parameters* `(10 5 0)
"
0: Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.

1: Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.

2: Determines how many spaces will be printed before each row
of a matrix (default 0)
")

(defgeneric print-element (x element stream)
  (:documentation "
  Syntax
  ======
  (PRINT-ELEMENT tensor element stream)

  Purpose
  =======
  This generic function is specialized to TENSOR to
  print ELEMENT to STREAM.  Called by PRINT-TENSOR/MATRIX
  to format a tensor into the STREAM."))

(defun print-tensor (tensor stream)
  (letv* ((rank (order tensor)) (dims (dimensions tensor))
	  (two-print-calls 0)
	  ((print-max-len print-max-args print-indent) *tensor-print-parameters*))
    (labels ((two-print (tensor subs)
	       (let* ((maxw (make-array (if (eq print-max-len t) (aref dims 1) (1+ print-max-len)) :initial-element 0))
		      (strs (iter (for i from 0 below (aref dims 0))
				  (if (or (eq print-max-len t) (< i print-max-len))
				      (collect (iter (for j from 0 below (aref dims 1))
						     (if (or (eq print-max-len t) (< j print-max-len))
							 (let ((str (with-output-to-string (str)
								      (print-element tensor (apply #'ref (list* tensor i j subs)) str))))
							   (collect str into cprints)
							   (setf (aref maxw j) (max (aref maxw j) (length str))))
							 (let ((str (with-output-to-string (str) (format str "..."))))
							   (collect str into cprints)
							   (setf (aref maxw j) (max (aref maxw j) (length str)))
							   (return cprints)))
						     (finally (return cprints)))
					into rprints)
				      (return rprints))
				  (finally (return rprints)))))
		 (iter (for row in strs)
		       (format stream (format nil "~~~AT" print-indent))
		       (iter (for cref in row)
			     (for j initially 0 then (1+ j))
			     (format stream (replace (make-string (+ (aref maxw j) 4) :initial-element #\Space) cref :start1 (if (char= (aref cref 0) #\-) 0 1))))
		       (format stream "~%"))
		 (unless (or (eq print-max-len t) (< (aref dims 0) print-max-len))
		   (format stream (format nil "~~~AT.~~%~~~:*~AT:~~%" print-indent)))))
	     (rec-print (tensor idx subs)
	       (if (>= idx 2)
		   (dotimes (i (aref dims idx) t)
		     (unless (rec-print tensor (1- idx) (append `(,i) subs))
		       (return nil)))
		   (progn
		     (if (or (eq print-max-args t) (< two-print-calls print-max-args))
			 (progn
			   (format stream "~A~%" (append '(\: \:) subs))
			   (two-print tensor subs)
			   (format stream "~%")
			   (incf two-print-calls)
			   t)
			 (progn
			   (format stream "~A~%" (make-list rank :initial-element '\:))
			   (format stream (format nil "~~~AT..~~%~~~AT::~~%" print-indent print-indent))
			   nil))))))
	(case rank
	  (1
	   (format stream (format nil "~~~AT" print-indent))
	   (dotimes (i (aref dims 0))
	     (if (or (eq print-max-len t) (< i print-max-len))
		 (progn
		   (print-element tensor (ref tensor i) stream)
		   (format stream "~,4T"))
		 (progn
		   (format stream "...")
		   (return nil))))
	   (format stream "~%"))
	  (2
	   (two-print tensor nil))
	  (t
	   (rec-print tensor (1- (order tensor)) nil))))))

(closer-mop:defmethod print-element ((x tensor) element stream)
  (cond
    ((floatp element) (format stream "~,4,-2,,,,'Eg" element))
    ((complexp element)
     (let ((realpart (cl:realpart element))
	   (imagpart (cl:imagpart element)))
       (if (not (zerop imagpart))
	   (format stream "~,4,-2,,,,'Eg ~a ~,4,-2,,,,'Egi"  realpart (if (>= imagpart 0) #\+ #\-) (abs imagpart))
	   (format stream "~,4,-2,,,,'Eg" realpart))))
    (t (format stream "~a" element))))

(closer-mop:defmethod print-object ((tensor tensor) stream)
  (if (typep tensor 'dense-tensor)
      (print-unreadable-object (tensor stream :type t)
	(format stream (string+ "~A" (if (slot-value tensor 'parent) "~,4T:DISPLACED" "")) (dimensions tensor))
	(when (> (total-size tensor) 0)
	  (format stream "~%")
	  (print-tensor tensor stream)))
      (print-unreadable-object (tensor stream :type t)
	(format stream "~A, size: ~A/~A" (dimensions tensor) (total-size tensor) (store-size tensor)))))

(closer-mop:defmethod print-object ((g graph-accessor) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A, size: ~A/~A" (dimensions g) (aref (fence g) (1- (length (fence g)))) (length (δ-i g)))))

