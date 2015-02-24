(in-package #:matlisp)

(declaim (ftype (function () double-float) draw-standard-normal draw-standard-exponential))

(definline draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution, which has density exp(-x)."
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; need 1-random, because there is a small but nonzero chance of getting a 0.
  (- (log (- 1d0 (random 1d0)))))

(definline draw-standard-normal ()
  "Draw a random number from N(0,1)."
  ;; Method from Leva (1992).  This is considered much better/faster than the Box-Muller method.
  ;; Adapted from cl-random, originally written by Tamas Papp
  ;; This seems to be just as fast as Marsaglia with storage.
  (very-quickly
    (loop
       :do (let* ((u (random 1d0))
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
(definline draw-standard-normal-marsaglia ()
  (very-quickly
    (loop :do
       (let* ((z (complex (1- (random 2d0)) (1- (random 2d0))))
	      (s (abs z)))
	 (declare (type (complex double-float) z)
		  (type double-float s))
	 (when (<= s 1)
	   (let ((mult (/ (* -4 (log s)) s)))
	     (declare (type double-float mult))
	     (return (* z mult))))))))

;;
(macrolet ((generate-rand (func clause)
	     (let ((clause (etypecase clause
			     (symbol `(,clause))
			     (cons clause)))
		   (func! (intern (string+ (symbol-name func) "!"))))
	       `(eval-every
		  (defun ,func! (tensor)
		    (declare (type ,(tensor 'double-float) tensor))
		    (very-quickly
		      (dorefs (idx (dimensions tensor))
			      ((ref tensor :type ,(tensor 'double-float)))
			      (setf ref ,clause)))
		    tensor)
		  (defun ,func (&optional dims)
		     (if dims
			 (,func! (zeros dims '(double-float)))
			 ,clause)))
	       ))
	   (generate-rands ((&rest args))
	     `(progn
		,@(mapcar #'(lambda (x) `(generate-rand ,(car x) ,(cadr x))) args))))
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
