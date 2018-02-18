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
			  (push x_i x_is))
	    (error () (finish))))
    (values x_is v0)))

(defun ziggurat-bisect (function r0 &key
			       (n-divisions (1+ (expt 2 8)))
			       (n-quadrature 128) (max-iterations 100)
			       (x_0-atol 1d-12) (r-atol 1d-14)
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

(defun ziggurat-tabulate (points function max-integer &optional (identity 1d0))
  (letv* ((ytab (mapcar #'(lambda (x) (funcall function (* identity x))) (butlast points)))
	  (wtab (mapcar #'(lambda (x) (* identity (* x (/ max-integer)))) (cdr points)))
	  (ktab (iter (for (x_i x_i+1) on points) (while x_i+1)
		      (collect (floor (* max-integer x_i (/ x_i+1)))))))
    (values-list
     (list*
      (coerce (elt points (1- (length points))) (type-of identity))
      (make-array (length ktab) :element-type (list 'unsigned-byte (* 32 (ceiling (log max-integer 2) 32)))
		  :initial-contents ktab)
      (mapcar #'(lambda (x) (make-array (length x) :element-type (type-of identity)
					:initial-contents x))
	      (list ytab wtab))))))

(defun ziggurat-compile (points function
			 uniform-sampler byte-sampler slow-sampler
			 &key (symmetricp t) (random-byte-size 32) (identity 1d0)
			 &aux (n-ziggurat (1- (length points))))
  (assert (= 0 (mod (log n-ziggurat 2) 1)) nil "N-ZIGGURAT (~a) is not a power of 2" n-ziggurat)
  (letv* ((log-nz (floor (log n-ziggurat 2)))
	  (log-nu (- random-byte-size log-nz (if symmetricp 1 0)))
	  (r0 ktab ytab wtab (ziggurat-tabulate points function (expt 2 log-nu) identity))
	  (f0 (funcall function r0)))
    (assert (< 0 log-nu) nil "insufficient RANDOM-BYTE-SIZE (~a)" random-byte-size)
    (using-gensyms (decl (f0 r0 ktab ytab wtab))
      `(let (,@decl)
	 (declare (type (simple-array (unsigned-byte ,random-byte-size) (,n-ziggurat)) ,ktab)
		  (type (simple-array ,(type-of identity) (,n-ziggurat)) ,wtab ,ytab)
		  (type ,(type-of identity) ,r0 ,f0))
	 (lambda ()
	   (loop
	      (let* ((rint (funcall ,byte-sampler ,(expt 2 random-byte-size)))
		     (ii (ldb ',(byte log-nz 0) rint))
		     (jj (ldb ',(byte log-nu log-nz) rint))
		     ,@(if symmetricp
			   `((ss (float (- 1 (* 2 (ldb ',(byte 1 (1- random-byte-size)) rint))) ,identity))))
		     (xx (* jj (aref ,wtab ii)))
		     (yy ,(* 0 identity)))
		(declare (type (unsigned-byte ,random-byte-size) rint)
			 (type (integer 0 ,(1- (expt 2 log-nz))) ii)
			 (type (integer 0 ,(1- (expt 2 log-nu))) jj)
			 (type ,(type-of identity) xx yy ,@(if symmetricp `(ss))))
		(when (< jj (aref ,ktab ii)) (return (* xx ,@(if symmetricp `(ss)))))
		(if (< ii ,(1- n-ziggurat))
		    (let ((y0 (aref ,ytab ii)) (y1 (aref ,ytab (1+ ii)))
			  (U1 (funcall ,uniform-sampler ,identity)))
		      (declare (type ,(type-of identity) y0 y1 U1))
		      (setf yy (+ y1 (* (- y0 y1) U1))))
		    (setf (values xx yy) (funcall ,slow-sampler ,r0 ,f0)))
		(when (< yy (funcall ,function xx)) (return (* xx ,@(if symmetricp `(ss))))))))))))

;; double
;; gsl_ran_gaussian_ziggurat (const gsl_rng * r, double sigma)
;; {
;;   unsigned long int i, j;
;;   int sign;
;;   double x, y;

;;   while (1)
;;     {
;;       i = gsl_rng_uniform_int (r, 256); /*  choose the step */
;;       j = gsl_rng_uniform_int (r, 16777216);  /* sample from 2^24 */
;;       sign = (i & 0x80) ? +1 : -1;
;;       i &= 0x7f;

;;       x = j * wtab[i];

;;       if (j < ktab[i])
;;         break;

;;       if (i < 127)
;;         {
;;           double y0, y1, U1;
;;           y0 = ytab[i];
;;           y1 = ytab[i + 1];
;;           U1 = gsl_rng_uniform (r);
;;           y = y1 + (y0 - y1) * U1;
;;         }
;;       else
;;         {
;;           double U1, U2;
;;           U1 = 1.0 - gsl_rng_uniform (r);
;;           U2 = gsl_rng_uniform (r);
;;           x = PARAM_R - log (U1) / PARAM_R;
;;           y = exp (-PARAM_R * (x - 0.5 * PARAM_R)) * U2;
;;         }

;;       if (y < exp (-0.5 * x * x))
;;         break;
;;     }

;;   return sign * sigma * x;
;;}
