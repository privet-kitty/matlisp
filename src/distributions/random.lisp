;; Copyright (c) 2018 Akshay Srinivasan

;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the
;; Free Software Foundation; either version 2.1 of the License, or (at your option)
;; any later version. Those exceptions, and interpretations specific to Lisp software,
;; as published by Franz Inc., shall take precedence over LGPL.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this
;; library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA. Clarifications on the applicability of LGPL to Lisp software
;; can be obtained from Franz Incorporated, Berkeley, CA 94704, USA.

(in-package #:matlisp)

(defun random-multinomial (probabilities &optional (n #.(expt 2 32)))
  (letv* ((cvec (iter (for pr-i in probabilities) (summing (floor (* pr-i n)) into zz)
		      (collect zz into ret)
		      (finally (return (coerce (cons 0 ret) 'simple-vector))))
		:type simple-vector)
	  (jj lb (binary-search (random (aref cvec (1- (length cvec)))) 0 (length cvec)
				(the simple-vector cvec))))
    (or jj (1- lb))))

;;
#+nil
(defmacro generate-rand (func type clause)
  (let ((clause (etypecase clause (symbol `(,clause)) (cons clause)))
	(func! (intern (string+ (symbol-name func) "!"))))
    `(eval-every
       (defun ,func! (tensor)
	 (declare (type ,(tensor type) tensor))
	 (very-quickly
	   (dorefs (idx (dimensions tensor))
		   ((ref tensor :type ,(tensor type)))
		   (setf ref ,clause)))
	 tensor)
       (defun ,func (&optional dims)
	 (if dims
	     (,func! (zeros dims ',(tensor type)))
	     ,clause)))))

#+nil
(macrolet ((generate-rands ((&rest args))
	     `(progn
		,@(mapcar #'(lambda (x) `(generate-rand ,(car x) double-float ,(cadr x))) args))))
  (generate-rands ((randn (draw-standard-normal))
		   (rand (random 1d0))
		   (rande (draw-standard-exponential)))))

(defun randi (&optional dims (arg 2))
  (if dims
      (let ((ret (zeros dims '(double-float))))
	(dorefs (idx (dimensions ret))
		((ref ret :type #.(tensor 'double-float)))
	  (setf ref (coerce (random arg) 'double-float)))
	ret)
      (random arg)))


