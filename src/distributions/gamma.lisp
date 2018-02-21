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
;;
(declaim (ftype (function (double-float double-float) double-float) random-gamma))

;; Borrowed from cl-randist
;; Implemented by J.D.Lamb@btinternet.com, minor modifications for GSL
;; by Brian Gough

(defun random-gamma-mt (a b)
  "Samples from the Gamma distribution of order a>0 defined by,
p(x) dx = {1 \over \Gamma(a) b^a} x^{a-1} e^{-x/b} dx, x > 0,
using the method of Marsaglia and Tsang [1].
  
[1]. 'A Simple Method for generating gamma variables', ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372."
  (declare (double-float a b))
  (if (< a 1d0)
      (* (random-gamma (+ 1d0 a) b) (expt (random 1d0) (/ a)))
      (let* ((x 0d0) (v 0d0) (u 0d0)
	     (d (- a (/ 3d0)))
 	     (c (/ (/ 3d0) (sqrt d))))
	(declare (double-float x v u d c))
	(tagbody
	 start
	   (setf x (random-normal)
		 v (+ 1d0 (* c x)))
	   (when (<= v 0d0) (go start))
	   ;;
	   (setf v (* v v v)
		 u (random 1d0))
	   (when (< u (- 1d0 (* 0.0331d0 x x x x))) (go end))
	   ;;
	   (when (< (log u) (+ (* 0.5d0 x x) (* d (+ 1 (- v) (the double-float (log v)))))) (go end))
	   ;;
	   (go start)
	 end)
	(* b d v))))

(definline random-gamma (a &optional (b 1d0))
  (random-gamma-mt (coerce a 'double-float) (coerce b 'double-float)))
