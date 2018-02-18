(in-package #:matlisp)

(definline draw-standard-exponential ()
  "Return a random variable from the Exponential(1) distribution, which has density exp(-x)."
  ;; Adapted from cl-random, originally written by Tamas Papp
  (- (log (- 1d0 (random 1d0)))))

;;
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

#+nil
(defun bin-sampler (ps &optional (n most-positive-fixnum) (state *mt-random-state*))
  (letv* ((cvec (iter (for pp in ps) (summing (floor (* pp n)) into zz)
		      (collect zz into ret)
		      (finally (return (coerce (cons 0 ret) 'simple-vector))))
		:type simple-vector)
	  (jj lb (binary-search (mt-random (aref cvec (1- (length cvec))) state) 0 (length cvec) (the simple-vector cvec))))
    (or jj (1- lb))))
