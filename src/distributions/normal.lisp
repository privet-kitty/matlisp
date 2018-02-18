(in-package #:matlisp)

(definline gaussian-function (x) (exp (* -1/2 x x)))

(definline gaussian-tail-sampler (r0 f0 identity)
  (let* ((U1 (- 1 (random identity)))
	 (U2 (- 1 (random identity)))
	 (x (- r0 (/ (log U1) r0)))
	 (y (* (exp (* (- identity) r0 (- x (/ r0 2 identity)))) U2)))
    (values x y)))

(defparameter *normal-ziggurat*
  (letv* ((points v (ziggurat-bisect #'gaussian-function 1d0 :n-divisions (expt 2 8)))
	  (points (cons 0d0 (cdr points))))
    (list points v)))

(declaim (ftype (function () double-float) draw-standard-normal-ziggurat))
(eval-when (:compile-toplevel)
  (setf (symbol-function 'draw-standard-normal-ziggurat)
	(compile-and-eval
	 (ziggurat-compile #'gaussian-function
			   (first *normal-ziggurat*) (second *normal-ziggurat*)
			   #'random #'random #'gaussian-tail-sampler))))

;; From from cl-random, originally written by Tamas Papp
(declaim (ftype (function () double-float) draw-standard-normal-leva))
(defun draw-standard-normal-leva ()
  "Draw a random number from N(0,1) using Leva's method [1]. This is considered much better/faster than the Box-Muller method.

[1] Leva, Joseph L. \"A fast normal random number generator.\" ACM Transactions on Mathematical Software (TOMS) 18.4 (1992): 449-453.

   Method from Leva (1992).
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

(declaim (ftype (function () (complex double-float)) draw-standard-normal-marsaglia))
(defun draw-standard-normal-marsaglia ()
  (very-quickly
    (loop
       (let* ((z (complex (1- (random 2d0)) (1- (random 2d0))))
	      (s (abs z)))
	 (declare (type (complex double-float) z)
		  (type double-float s))
	 (when (<= s 1)
	   (let ((mult (/ (* -4 (log s)) s)))
	     (declare (type double-float mult))
	     (return (* z mult))))))))
