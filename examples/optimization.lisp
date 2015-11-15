(in-package :matlisp)
(in-readtable :infix-dispatch-table)

(define-condition exceeded-maximum-iterations (warning)
  ((message :initarg :message))
  (:report (lambda (c stream)
	     (when (slot-boundp c 'message)
	       (format stream "~a~%" (slot-value c 'message))))))
;;
(defun backtracking-linesearch (x dx f df·dx func &key (t_0 1.0d0) (c 0.5) (ρ 0.5) (max-iterations 20))
  "
Implements the Backtracking linesearch at $x \in \mathbb{R}^n$:

choose t_0 > 0, ρ ∈ (0, 1), c ∈ (0, 1),
t ← t_0
repeat until
f(x + t * dx) ≤ f(x_k) + c t (df · dx)
  t ← ρ * t
end
  "
  (let ((xnew (zeros (dimensions x) (class-of x))))
    (iter (counting t into k)
	  (for tk first t_0 then (* tk ρ))
	  (copy! x xnew) (axpy! tk dx xnew)
	  (when (<= (funcall func xnew 0) (+ f (* c tk df·dx))) (finish))
	  (when (>= k max-iterations)
	    (restart-case (warn 'exceeded-maximum-iterations :message "Backtracking failed.")
	      (continue-linesearch (num-iterations) (when num-iterations (incf max-iterations num-iterations) (next-iteration))))
	    (finish))
	  (finally (return (values xnew tk k))))))

(defun rosenbrock (q &optional (grad? 0))
  (with-coordinates (x y) q
    (values-n (1+ grad?)
	      #i((1 - x)^2 + 100 * (y - x^2)^2)
	      #d[-2 * (1-x) - 100 * 2 * (y - x^2) * 2 * x, 2 * 100 * (y - x^2)])))
;;
(defmacro define-optimizer (name (&whole args state function &optional max-iterations) updator)
  (letv* (((up.sym up.args &body up.body) updator :type ((eql :updator) list list))
	  (up.args aux-dyn (let ((aux-pos (position '&aux-dynamic up.args)))
			     (values (subseq up.args 0 aux-pos) (subseq up.args (1+ aux-pos))))))
    (with-gensyms (kk)
      `(defun ,name (,@args ,@up.args ,@(if (not (member '&key up.args)) `(&key)) (max-iterations ,(or max-iterations 10)))
	 (letv* ((,state (copy ,state))
		 ,@aux-dyn)	   
	   (iter (counting t into ,kk)
		 (progn ,@up.body)
		 (when (>= ,kk max-iterations)
		   (restart-case (warn 'exceeded-maximum-iterations :message "Backtracking failed.")
		     (continue-optimization (num-iterations) (when num-iterations (incf max-iterations num-iterations) (next-iteration))))
		   (finish))
		 (finally (return (values ,state ,kk)))))))))

(define-optimizer gradient-descent (x func)
  (:updator (&key (atol 1d-6) &aux-dynamic (tk 1d0))
     (letv* ((f df (funcall func x 1)))
       (if (< (norm df) atol) (finish)
	   (letv* ((x+ tk+ nbk (backtracking-linesearch x (scal! -1 df) f (- (dot df df)) func :t_0 (* 2 tk) :c 0.5 :ρ 0.5 :max-iterations 20) :type t t nil))
	     (copy! x+ x) (setf tk tk+))))))

;;SR1 algorithm
(defun sr1-update! (alpha sk yk Hkp &optional (r 1d-6))
  (let* ((eta (gem! -1 Hkp yk alpha (copy sk)))
	 (eta.y (dot eta yk)))
    (when (> (abs eta.y) (* r (norm eta) (norm yk)))
      (ger! (/ eta.y) eta eta Hkp))
    Hkp))

(defun lsr1-update! (sk yk buf &optional push? (r 1d-6))
  (let* ((eta (axpy! 1 sk (lsr1-query! (copy yk) buf)))
	 (eta.y (dot eta yk)))
    (when (> (abs eta.y) (* r (norm eta) (norm yk)))
      (let* ((lbuf (if push? (dlist:dpush (cons nil 0d0) buf) (first buf)))
	     (bcon (cddr lbuf)))
	(setf (car bcon) eta
	      (cdr bcon) eta.y)
	lbuf))))

(defun lsr1-query! (q buf &optional H0)
  (let ((alpha nil))
    (iter (for (eta . rho) in-dlist buf)
	  (push (dot eta q) alpha))
    (when H0 (copy! (gem! 1 H0 q nil nil) q))
    (iter (for a.k in alpha)
	  (for (eta . rho) in-dlist (dlist:drdc buf) in-reverse t)
	  (axpy! (/ a.k rho) eta q)))
  (scal! -1 q))

(define-optimizer sr1-descent (x func 100)
  (:updator (&key (atol 1d-6) &aux-dynamic (tk 1d0)
		  (yk dk Hk (let ((n (dimensions x 0)))
			      (values (zeros n (class-of x)) (zeros n (class-of x)) (eye (list n n) (class-of x))))))
     (letv* (()))))

(defun sr1-descent (x0 func &key (atol 1d-6) (max-iterations 100))
  (let ((xk (copy x0)) (tk 1d0)
	(yk (zeros (dimensions x0))) (dk (zeros (dimensions x0)))
	(Hk (eye (make-list 2 :initial-element (dimensions x0 0))))
	(niters 0))
    (iter (for it from 0 below max-iterations)
	  (multiple-value-bind (f df) (funcall func xk 1)
	    (when (< (norm df) atol) (finish))
	    (when (> it 0) (sr1-update! tk dk (axpy! 1 df yk) Hk))
	    (gemv! -1 Hk df 0 dk)
	    (let ((g.d (dot dk df)))
	      (when (> g.d 0) (scal! -1 dk) (setf g.d (- g.d)))
	      (setq tk (nth-value 1 (backtracking-linesearch xk dk f g.d func :t0 1d0 :c 0.1 :rho 0.5 :max-iterations 10))))
	    (copy! df yk) (scal! -1 yk))
	  (finally (incf niters it)
		   (when (= it max-iterations)
		     (warn 'exceeded-maximum-iterations :message "SR1 exceeded max-iterations."))))
    (values xk niters)))

;;Broyden–Fletcher–Goldfarb–Shanno algorithm

;;Must use a different line-search to ensure s · y > 0
(defun bfgs-update! (alpha sk yk Hkp)
  ;;B^+ = P' * B * P + rho .* sk ^ sk
  ;;rho = 1/(sk @ yk)
  ;;P = (id - rho .* yk ^ sk)
  (let ((rho (/ (dot sk yk))))
    (ger! (- rho) (gem 1 Hkp yk nil nil) sk Hkp)     ;;r    (ger! (- rho) sk (gemv! 1 Hkp yk 0 tmp :t) Hkp)  ;;left
    (ger! (* alpha rho) sk sk Hkp))
  Hkp)

;;Use a proper circular buffer instead of using dlists ?
(defun lbfgs-query! (q buf &optional H0)
  (let ((alpha nil))
    (iter (for (y.k s.k . rho.k) in-dlist buf)
	  (let ((a.k (* rho.k (dot s.k q))))
	    (push a.k alpha)
	    (axpy! (- a.k) y.k q)))
    (when H0 (copy! (gem 1 H0 q nil nil) q))
    (iter (for a.k in alpha)
	  (for (y.k s.k . rho.k) in-dlist (dlist:drdc buf) in-reverse t)
	  (axpy! (- a.k (* rho.k (dot y.k q))) s.k q)))
  q)

(defun lbfgs-update! (sk yk buf &optional extend?)
  (letv* ((buf (if extend?
		   (dlist:dpush (list (zeros (dimensions sk) (class-of sk)) (zeros (dimensions yk) (class-of yk))) buf)
		   (dlist:drdc buf)))
	  ((&whole buf-k yi si . ρi) (dlist:dcar buf)))
    (copy! yk yi) (copy! sk si)
    (setf (cddr buf-k) (/ (dot sk yk)))
    buf))
;;
(defun bfgs-descent (x0 func &key (atol 1d-6) (max-iterations 100) buffer-size)
  (macrolet ((outline (sparse?) `(let ((xk (copy x0)) (tk 1d0)
				       (yk (zeros (dimensions x0) (class-of x0))) (dxk (zeros (dimensions x0) (class-of x0)))
				       (Hk ,(if sparse? nil `(eye (list (dimensions x0 0) (dimensions x0 0)) (class-of x0))))
				       (Hk-count 0))
				   (iter (counting t into k)
					 (letv* ((f df (funcall func xk 1)))
					   (unless (first-time-p)
					     ,@(if sparse?
						   `((bfgs-update! tk dxk (axpy! 1 df yk) Hk)
						     (gem! -1 Hk df 0 dxk))
						   `((setf Hk (lbfgs-update! (scal! tk dxk) (axpy! 1 df yk) Hk (when (< Hk-count buffer-size) (incf Hk-count))))
						     (lbfgs-query! (copy! df (scal! -1 dxk)) Hk))))
					   (scal! -1 (copy! df yk))
					   (if (< (norm df) atol) (finish)
					       (letv* ((x+ tk+ nbk (backtracking-linesearch xk dxk f (dot dxk df) func :t_0 1d0 :c 0.1 :ρ 0.5 :max-iterations 10) :type t t nil))
						 (copy! x+ xk) (setf tk tk+))))
					 (when (>= k max-iterations)
					   (restart-case (warn 'exceeded-maximum-iterations :message "BFGS exceeded max-iterations.")
					     (continue-optimization (num-iterations) (when num-iterations (incf max-iterations num-iterations) (next-iteration))))
					   (finish))
					 (finally (return (values xk k)))))))
    (if buffer-size (outline t) (outline nil))))

(defun )

#+nil
(define-optimizer bfgs-descent (x0 func)
  (:updator (x f df &aux-dynamic
	       (tk 1) (yk (zeros (dimensions x0) (class-of x0))) (dxk (zeros (dimensions x0) (class-of x0)))
	       (Hk (eye (list (dimensions x0) (dimensions x0)) (class-of x0))))
    (unless (first-time-p) (bfgs-update! tk dxk (axpy! 1 df yk) Hk) (gem! -1 Hk df 0 dxk))
    (scal! -1 (copy! df yk))
    (letv* ((x+ tk+ nbk (backtracking-linesearch x (scal! -1 df) f (- (dot df df)) func :t_0 (* 2 tk) :c 0.5 :ρ 0.5 :max-iterations 20) :type t t nil))
      (copy! x+ x) (setf tk tk+))))
;;



;;
(defun cauchy (delta g H)
  (let* ((H.g #i(H * g)) (gHg (dot g H.g))
	 (ng (norm g)) (plen (/ delta ng)))
    (scal! (if (<= gHg 0) plen (min (/ (* ng ng) gHg) plen)) (copy! g H.g))))


(defun lconstraint (q &optional (grad? 0))
  (with-coordinates (x y) q
    (values-n (1+ grad?)
	      #d[x**2 + y**2 - 1]
	      #d[[2 * x, 2 * y]])))

(defun auglag (x0 func c0 cfunc mu &key (atol 1d-6) (max-iterations 10))
  (let ((xk (copy x0))
	(ck (copy c0)))
    (iter (repeat max-iterations)
	  (copy! (sr1-descent xk #'(lambda (x &optional (grad? 0))
				     (letv* ((f df (funcall func x grad?))
					     (c dc (funcall cfunc x grad?)))
					    (values-n (1+ grad?)
						      #i(f - ck @ c + 0.5d0 / mu * c @ c)
						      #i(df - (ck - c/mu) * dc)))) :atol atol)
		 xk)
	  #i(ck[] = ck - 1/mu * funcall(cfunc, xk, 0))
	  (setf mu (/ mu 2)))
    (values ck xk mu)))

;;

(defun hz-linesearch ())


