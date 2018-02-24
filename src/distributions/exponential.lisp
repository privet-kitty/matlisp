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

;; exponential distribution
(eval-when (:compile-toplevel :load-toplevel :execute)
  (definline exponential-function (x) (exp (- x)))
  (definline exponential-tail-sampler (r0 f0 identity)
    (let* ((u1 (random-uniform nil t)))
      (+ r0 (* -1 (log u1))))))

(declaim (ftype (function () double-float) random-exponential-kernel))
(eval-when (:compile-toplevel)
  (definline exponential-function (x) (exp (- x)))
  (definline exponential-tail-sampler (r0 f0 identity)
    (let* ((u1 (random-uniform nil t)))
      (+ r0 (* -1 (log u1)))))
  ;;
  (letv* ((points v (ziggurat-bisect #'exponential-function 15d0 :n-divisions (expt 2 8) :x_0-atol 1d-14))
	  (points (cons 0d0 (cdr points))))
    (setf (symbol-function 'random-exponential-kernel)
	  (compile-and-eval
	   (ziggurat-compile #'exponential-function points v
			     #'(lambda (arg) (random-uniform nil arg))
			     #'random-byte-kernel #'exponential-tail-sampler
			     :symmetricp nil)))))

(defrandom random-exponential (&optional (beta 1))
  "
  Returns a random sample from the exponential distribution defined by, 
  p(x) dx = exp(- x)
  using the Ziggurat method of Marsaglia and Tsang [1].

  [1] Marsaglia, George, and Wai Wan Tsang. \"The ziggurat method for generating random variables.\" Journal of statistical software 5.8 (2000): 1-7.
"
  (* (coerce beta 'double-float) (random-exponential-kernel)))

;; normal distribution
(eval-when (:compile-toplevel :load-toplevel :execute)
  (definline gaussian-function (x) (exp (* -1/2 x x)))
  (definline gaussian-tail-sampler (r0 f0 identity)
    (let* ((u1 (random-uniform nil t))
	   (u2 (random-uniform nil t))
	   (x (* -1 (/ r0) (log u1)))
	   (y (- 1 (log u2))))
      (if (> (+ y y) (* x x)) (+ x r0)))))

(declaim (ftype (function () double-float) random-normal-kernel))
(eval-when (:compile-toplevel)
  (letv* ((points v (ziggurat-bisect #'gaussian-function 1d0 :n-divisions (expt 2 7)))
	  (points (cons 0d0 (cdr points))))
    (setf (symbol-function 'random-normal-kernel)
	  (compile-and-eval
	   (ziggurat-compile #'gaussian-function points v
			     #'(lambda (arg) (random-uniform nil arg))
			     #'random-byte-kernel #'gaussian-tail-sampler)))))

(defrandom random-normal (&optional (mean 0) (std 1))
  "
  Returns a random sample from the Normal distribution N(0, 1), defined by, 
  p(x) dx = {1 \over \sqrt{2 \pi}} exp(- x^2 / 2)
  using the Ziggurat method of Marsaglia and Tsang [1].

  [1] Marsaglia, George, and Wai Wan Tsang. \"The ziggurat method for generating random variables.\" Journal of statistical software 5.8 (2000): 1-7.
"
  (+ (coerce mean 'double-float) (* (coerce std 'double-float) (random-normal-kernel))))

;; gamma distribution
(declaim (ftype (function (double-float) double-float) random-gamma-kernel))
(defun random-gamma-kernel (a)
  "
  Samples from the Gamma distribution of order a > 1 defined by,
  p(x) dx = {1 \over \Gamma(a)} x^{a-1} e^{-x} dx, x > 0,
  using the method of Marsaglia and Tsang [1].

  [1]. 'A Simple Method for generating gamma variables', ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372.

  Implemented by J.D.Lamb@btinternet.com, minor modifications for GSL by Brian Gough.

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"  
  (declare (type double-float a))
  (let* ((x 0d0) (v 0d0) (u 0d0)
	 (d (- a (/ 3d0)))
	 (c (/ (/ 3d0) (the double-float (sqrt d)))))
    (declare (double-float x v u d c))
    (tagbody
     start
       (setf x (random-normal)
	     v (+ 1d0 (* c x)))
       ;;check validity
       (when (<= v 0d0) (go start))
       (setf v (* v v v)
	     u (random-uniform))
       ;;squeeze check
       (when (< u (- 1d0 (* 0.0331d0 x x x x))) (go end))
       ;;slow check
       (when (< (log u) (+ (* 1/2 x x) (* d (+ 1 (- v) (the double-float (log v)))))) (go end))
       ;;rinse-repeat
       (go start)
     end)
    (* d v)))

(defrandom random-gamma (a &optional (b 1))
  "
  Returns a sample from the Gamma distribution of order a>0 defined by,
  p(x) dx = {1 \over \Gamma(a) b^a} x^{a-1} e^{-x/b} dx, x > 0,
  using the method of Marsaglia and Tsang [1].

  [1]. 'A Simple Method for generating gamma variables', ACM Transactions on Mathematical Software, Vol 26, No 3 (2000), p363-372.

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"
  (let ((a (coerce a 'double-float))
	(b (coerce b 'double-float)))
    (declare (type double-float a b))
    (if (< a 1d0)
	(* b (random-gamma-kernel (+ 1d0 a)) (the double-float (expt (random-uniform nil) (/ a))))
	(* b (random-gamma-kernel a)))))

;;beta distribution
(defrandom random-beta (a b)
  "
  The beta distribution has the form
  p(x) dx = (Gamma(a + b)/(Gamma(a) Gamma(b))) x^(a-1) (1-x)^(b-1) dx
  The method used here is the one described in Knuth.

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"
  (let ((x1 (random-gamma a))
	(x2 (random-gamma b)))
    (declare (double-float x1 x2))
    (/ x1 (+ x1 x2))))

;;chi-square
(defrandom random-chi-square (nu)
  "
  Generate random variable for chi square distribution:
  p(x) dx = (1/(2*Gamma(nu/2))) (x/2)^(nu/2 - 1) exp(-x/2) dx

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"
  (* 2d0 (random-gamma (/ (coerce nu 'double-float) 2d0))))

;;t distribution
(declaim (ftype (function (double-float) double-float) random-t-kernel))    
(defun random-t-kernel (nu)
  "
  The t-distribution has the form,

  p(x) dx = (Gamma((nu + 1)/2)/(sqrt(pi nu) Gamma(nu/2)) * (1 + (x^2)/nu)^-((nu + 1)/2) dx

  The method used here is the one described in Knuth

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"
  (declare (type double-float nu))
  (if (<= nu 2d0)
      (let ((y1 (random-normal))
	    (y2 (random-chi-square nu)))
	(/ y1 (sqrt (/ y2 nu))))
      (let ((y1 0d0) (y2 0d0) (Z 0d0))
	(declare (type double-float y1 y2 Z))
	(tagbody
	 start
	   (setf y1 (random-normal))
	   (setf y2 (random-exponential nil (/ (- (/ nu 2d0) 1d0))))
	   (setf z (/ (* y1 y1) (- nu 2d0)))
	   (when (or (< (- 1d0 z) 0)
		     (> (exp (- (- y2) z)) (- 1d0 z)))
	     (go start)))
	(/ y1 (sqrt (* (- 1d0 (/ 2d0 nu)) (- 1d0 z)))))))

(defrandom random-t (nu)
  "
  Generates a random variable from the t-distribution defined by, 
  p(x) dx = (Gamma((nu + 1)/2)/(sqrt(pi nu) Gamma(nu/2)) * (1 + (x^2)/nu)^-((nu + 1)/2) dx

  Originally from cl-randist by Leonardo Varuzza et.al.
  https://github.com/lvaruzza/cl-randist
"
  (random-t-kernel (coerce nu 'double-float)))
;;

#|
(defun random-dirichlet-kernel (alpha theta)
  "The Dirichlet probability distribution of order K-1 is

     p(\theta_1,...,\theta_K) d\theta_1 ... d\theta_K =
	(1/Z) \prod_i=1,K \theta_i^{alpha_i - 1} \delta(1 -\sum_i=1,K \theta_i)

   The normalization factor Z can be expressed in terms of gamma functions:

      Z = {\prod_i=1,K \Gamma(\alpha_i)} / {\Gamma( \sum_i=1,K \alpha_i)}

   The K constants, \alpha_1,...,\alpha_K, must be positive. The K parameters,
   \theta_1,...,\theta_K are nonnegative and sum to 1.

   The random variates are generated by sampling K values from gamma
   distributions with parameters a=\alpha_i, b=1, and renormalizing.
   See A.M. Law, W.D. Kelton, Simulation Modeling and Analysis (1991).

   Gavin E. Crooks <gec@compbio.berkeley.edu> (2002)
 "
  (let ((norm 0.0)
	(k (array-dimension alpha 0)))
    (loop
       :for i :from 0 :to (1- k)
       :for gamma := (random-gamma (coerce (aref alpha i)  'double-float) 1.0d0)
       :sum gamma :into N
       :do (setf (aref theta i) gamma)
       :finally (setf norm N))
    (loop
       :for i :from 0 :to (1- k)
       :do (setf (aref theta i) (/ (aref theta i) norm)))
    theta))
|#
