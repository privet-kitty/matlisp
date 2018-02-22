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

(defun cauchy-riemann-derivative (f x &optional (eps 1d-15))
  (let ((ret (funcall f (complex x eps))))
    (values (cl:realpart ret) (/ (cl:imagpart ret) eps))))

(defun newton-solve (function x0 &key (atol 1d-15) (max-iterations 100) (step-size 1))
  (iter (for _ii from 0 below max-iterations)
	(letv* ((f df (cauchy-riemann-derivative function x0)))
	  (setf x0 (- x0 (* step-size (/ f df))))
	  (if (< (abs f) atol) (finish))))
  (values x0 (funcall function x0)))

(defun tail-integrate (function r quadrature)
  (letv* (((s w) quadrature))
    (+ (* r (funcall function r))
       (iter (for ii below (dimensions s 0))
	     (summing (* (ref w ii) (exp (ref s ii)) (funcall function (+ r (ref s ii)))))))))

(defun ziggurat-solve (function x_i+1 v)
  (newton-solve (let ((f_i+1 (funcall function x_i+1)))
		  #'(lambda (x) (- v (* x_i+1 (- (funcall function x) f_i+1)))))
		x_i+1))

(defun ziggurat-generate (function r quadrature num-divisions &key (atol 1d-15))
  "Generates (x_i, f_i) values that form the ziggurat [1].

[1] Marsaglia, George, and Wai Wan Tsang. \"The ziggurat method for generating random variables.\" Journal of statistical software 5.8 (2000): 1-7.
"
  (letv* ((v0 (tail-integrate function r quadrature))
	  (x_is (list r)))
    (iter (repeat (1- num-divisions))
	  (handler-case (letv* ((x_i err_i (ziggurat-solve function (car x_is) v0)))
			  (when (< atol (abs err_i)) (error "exceeded atol"))
			  (when (< x_i 0) (error "left of origin"))
			  (push x_i x_is))
	    (error () (finish))))
    (values x_is v0)))

(defun ziggurat-bisect (function r0
			&key
			  (n-divisions (1+ (expt 2 8)))
			  (n-quadrature 128) (max-iterations 100)
			  (x_0-atol 1d-15) (r-atol 1d-15)
			&aux
			  (quadrature (multiple-value-list (gauss-quadrature n-quadrature :laguerre)))
			  (rl 0d0) (rh nil))
  (labels ((zig (r) (letv* ((x_is (ziggurat-generate function r quadrature n-divisions))
			    (n-solve (length x_is)))
		      (if (and (= n-solve n-divisions) (<= 0d0 (car x_is))) (car x_is)))))
    (iter (repeat max-iterations)
	  (letv* ((r (if rh (* 0.5d0 (+ rl rh)) r0))
		  (x0 (zig r)))
	    ;;(print (list rl r rh x0))
	    (cond
	      ((not x0)
	       (if (not rh)
		   (setf (values rl r0) (values r (1+ (* 2d0 r)))) ;; initialize
		   (setf (values rl rh) (values r rh)))) ;; update
	      ((or (and rh (< (- rh rl) r-atol))
		   (< x0 x_0-atol))
	       (return (ziggurat-generate function r quadrature n-divisions)))
	      (t (setf (values rl rh) (values rl r))))))))

(defun ziggurat-tabulate (function points v max-integer &aux (n-points (length points)))
  (letv* ((ytab (make-array n-points :element-type (type-of v)
			    :initial-contents (mapcar #'(lambda (x) (funcall function (coerce x (type-of v)))) points)))
	  (wtab (make-array n-points :element-type (type-of v)
			    :initial-contents (mapcar #'(lambda (x) (coerce (* x (/ max-integer)) (type-of v)))
						      (append (cdr points) (list (* v (/ (aref ytab (1- n-points)))))))))
	  (ktab (iter (for (x_i x_i+1) on points)
		      (collect (floor (* max-integer x_i (if x_i+1 (/ x_i+1) (/ (aref ytab (1- n-points)) v))))))))
    ;;(collect (floor (* max-integer x_i (/ x_i+1))))
    (values-list
     (list*
      (make-array (length ktab) :element-type (list 'unsigned-byte (* 32 (ceiling (log max-integer 2) 32)))
		  :initial-contents ktab)
      (mapcar #'(lambda (x) (make-array (length x) :element-type (type-of v)
					:initial-contents x))
	      (list ytab wtab))))))

(defun ziggurat-compile (function points v
			 uniform-sampler byte-sampler tail-sampler
			 &key (symmetricp t) (random-byte-size 32)
			 &aux (n-ziggurat (length points)))
  (assert (= 0 (mod (log n-ziggurat 2) 1)) nil "N-ZIGGURAT (~a) is not a power of 2" n-ziggurat)
  (letv* ((log-nz (floor (log n-ziggurat 2)))
	  (log-nu (- random-byte-size log-nz (if symmetricp 1 0)))
	  (ktab ytab wtab (ziggurat-tabulate function points v (expt 2 log-nu)))
	  (r0 (elt points (1- (length points))))
	  (f0 (funcall function r0)))
    (assert (< 0 log-nu) nil "insufficient RANDOM-BYTE-SIZE (~a)" random-byte-size)
    (using-gensyms (decl (f0 r0 ktab ytab wtab))
      `(let (,@decl)
	 (declare (type (simple-array (unsigned-byte ,random-byte-size) (,n-ziggurat)) ,ktab)
		  (type (simple-array ,(type-of v) (,n-ziggurat)) ,wtab ,ytab)
		  (type ,(type-of v) ,r0 ,f0))
	 (lambda ()
	    ;;(declare (optimize (speed 3) (space 0) (safety 0)))
	    (loop
	       (let* ((rint (funcall ,byte-sampler ,(expt 2 random-byte-size)))
		      (ii (ldb ',(byte log-nz 0) rint))
		      (jj (ldb ',(byte log-nu log-nz) rint))
		      ,@(if symmetricp
			    `((ss (float (- 1 (* 2 (ldb ',(byte 1 (1- random-byte-size)) rint))) ,(coerce 1 (type-of v))))))
		      (xx (* jj (aref ,wtab ii))))
		 (declare (type (unsigned-byte ,random-byte-size) rint)
			  (type (integer 0 ,(1- (expt 2 log-nz))) ii)
			  (type (integer 0 ,(1- (expt 2 log-nu))) jj)
			  (type ,(type-of v) xx ,@(if symmetricp `(ss))))
		 (when (< jj (aref ,ktab ii)) (return (* xx ,@(if symmetricp `(ss)))))
		 (if (< ii ,(1- n-ziggurat))
		     (let* ((y0 (aref ,ytab ii)) (y1 (aref ,ytab (1+ ii)))
			    (U1 (funcall ,uniform-sampler ,(coerce 1 (type-of v))))
			    (yy (+ y1 (* (- y0 y1) U1))))
		       (declare (type ,(type-of v) y0 y1 U1 yy))
		       (when (< yy (the ,(type-of v) (funcall ,function xx))) (return (* xx ,@(if symmetricp `(ss))))))
		     (let ((xx (funcall ,tail-sampler ,r0 ,f0 ,(coerce 1 (type-of v)))))
		       (declare (type (or null ,(type-of v)) xx))
		       (when xx (return (* xx ,@(if symmetricp `(ss))))))))))))))
