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

(definline gaussian-function (x) (exp (* -1/2 x x)))
(definline gaussian-tail-sampler (r0 f0 identity)
  (let* ((u1 (- 1 (random identity)))
	 (u2 (- 1 (random identity)))
	 (x (* -1 (/ r0) (log u1)))
	 (y (- 1 (log u2))))
    (if (> (+ y y) (* x x)) (+ x r0))))

(declaim (ftype (function () double-float) random-normal))
(eval-when (:compile-toplevel)
  (letv* ((points v (ziggurat-bisect #'gaussian-function 1d0 :n-divisions (expt 2 7)))
	  (points (cons 0d0 (cdr points))))
    (setf (symbol-function 'random-normal)
	  (compile-and-eval
	   (ziggurat-compile #'gaussian-function points v
			     #'random #'random #'gaussian-tail-sampler)))))

#+nil
(definline gaussian-tail-sampler (r0 f0 identity)
  (let* ((u1 (- 1 (random identity)))
	 (u2 (- 1 (random identity)))
	 (xx (sqrt (- (* r0 r0) (* 2 (log u1))))))
    (if (< u2 (/ r0 xx)) xx)))

;; From from cl-random, originally written by Tamas Papp
#|
(declaim (ftype (function () double-float) random-normal-leva))
(defun random-normal-leva ()
  "Draw a random number from N(0,1) using Leva's method [1]. This is considered much better/faster than the Box-Muller method.

[1] Leva, Joseph L. \"A fast normal random number generator.\" ACM Transactions on Mathematical Software (TOMS) 18.4 (1992): 449-453.
  "
  (very-quickly
    (loop
       (let* ((u (random 1d0))
	      (v (* 1.7156d0 (- (random 1d0) 0.5d0)))
	      (x (- u 0.449871d0))
	      (y (+ (abs v) 0.386595d0))
	      (q (+ (expt x 2) (* y (- (* 0.19600d0 y) (* 0.25472d0 x))))))
	 (declare (type double-float u v x y q))
	 (unless (and (> q 0.27597d0)
		      (or (> q 0.27846d0)
			  (plusp (+ (expt v 2) (* 4 (expt u 2) (log u))))))
	   (return (/ v u)))))))

(declaim (ftype (function () (values double-float double-float)) random-normal-marsaglia))
(defun random-normal-marsaglia ()
  (very-quickly
    (loop
       (let* ((x (1- (random 2d0)))
	      (y  (1- (random 2d0)))
	      (s (+ (* x x) (* y y))))
	 (declare (type double-float x y s))
	 (when (<= s 1)
	   (let ((mult (sqrt (* -2 (log s) (/ s)))))
	     (declare (type double-float mult))
	     (return (values (* x mult) (* y mult)))))))))
|#
