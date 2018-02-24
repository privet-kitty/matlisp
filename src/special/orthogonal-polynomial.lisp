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

(in-package :matlisp)
(in-readtable :infix-dispatch-table)

(defclass orthogonal-polynomial ()
  (an bn cn (p0 :initform 1) v0 (vn :initform nil))
  (:documentation
   "P_{n} = (a_n x + b_n) P_{n - 1} - c_n P_{n - 2}; P_{-1} = 0
    v_0 = (P_0, P_0)_w"))

(defmacro make-orthogonal-polynomial (&rest body)
  (with-gensyms (ret)
    `(let ((,ret (make-instance 'orthogonal-polynomial)))
       (set-slots ,ret
	 ,@(mapcar #'(lambda (x)
		       (match x
			 ((list* (and name (or :an :bn :cn)) (list n) body)
			  `(,(find-symbol (symbol-name name)) (lambda (,n) (declare (ignorable ,n))
								      (assert (<= 1 ,n) nil "coefficient ~a only for n >= 1" name)
								      (cond ,@body))))
			 ((list (and name (or :p0 :v0)) value)
			  `(,(find-symbol (symbol-name name)) ,value))))
		   body))
       (iter (for _ss in (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (find-class 'orthogonal-polynomial))))
	     (assert (slot-boundp ,ret _ss) nil "missing slot: ~a" _ss))
       ,ret)))
;;
(with-memoization ()
  (memoizing
   (defun get-orthogonal-polynomial (name)
     (match name
       (:chebyshev-t
	;;domain: [-1, 1]
	;;weight: {1 \over \sqrt(1 - x^2) }
	(make-orthogonal-polynomial
	 (:an (n)
	      ((= 1 n) 1)
	      ((<= 2 n) 2))
	 (:bn (n) (t 0))
	 (:cn (n) (t 1))
	 (:p0 1) (:v0 pi)))
       (:chebyshev-u
	;;domain: [-1, 1]
	;;weight: \sqrt(1 - x^2)
	(make-orthogonal-polynomial
	 (:an (n) ((<= 1 n) 2))
	 (:bn (n) (t 0))
	 (:cn (n) (t 1))
	 (:p0 1) (:v0 (/ pi 2))))
       (:legendre
	;;domain: [-1, 1]
	;;weight: 1
	(make-orthogonal-polynomial
	 (:an (n)
	      ((= n 1) 1)
	      (t (- 2 (/ 1 n))))
	 (:bn (n) (t 0))
	 (:cn (n) (t (- 1 (/ 1 n))))
	 (:p0 1) (:v0 2)))
       (:laguerre (get-orthogonal-polynomial (list :laguerre 0)))
       ((list :laguerre alpha)
	;;domain: [0, \infty)
	;;weight: x^{alpha} \exp(-x)
	(make-orthogonal-polynomial
	 (:an (n) ((<= 1 n) (- (/ n))))
	 (:bn (n) ((<= 1 n) (+ 2 (/ (1- alpha) n))))
	 (:cn (n) ((<= 2 n) (+ 1 (/ (1- alpha) n))))
	 (:p0 1) (:v0 1)))
       (:hermite-e
	;;domain: (-\infty, \infty)
	;;weight: \exp(-x^2 / 2)
	(make-orthogonal-polynomial
	 (:an (n) (t 1))
	 (:bn (n) (t 0))
	 (:cn (n) (t (1- n)))
	 (:p0 1) (:v0 (sqrt (* 2 pi)))))
       (:hermite
	;;domain: (-\infty, \infty)
	;;weight: \exp(-x^2)
	(make-orthogonal-polynomial
	 (:an (n) (t 2))
	 (:bn (n) (t 0))
	 (:cn (n) (t (* 2 (1- n))))
	 (:p0 1) (:v0 (sqrt pi))))))))
;;
(defun alpha-n (n p)
  "x P_n = \alpha_n P_{n - 1} + \beta_{n + 1} P_{n} + \alpha_{n + 1} P_{n + 1}.
where P_n are orthonormal.

The recurrence relation for the orthonormal family can be obtained using the following correspondence,
\alpha_n = \sqrt{c_{n + 1} \over a_{n + 1} a_n}.
\beta_n = {-b_n \over a_n}.
"
  (cond
    #+nil
    ((<= 1 n) (* (sqrt (/ (norm-square n p) (norm-square (1- n) p)))
		 (/ (funcall #i(p.an) n))))
    ((<= 1 n) (sqrt (/ (funcall #i(p.cn) (1+ n))
		       (funcall #i(p.an) (1+ n))
		       (funcall #i(p.an) n)
		       1d0)))
    (t (error "alpha-n only defined for n >= 1"))))

(defun beta-n (n p)
    "x P_n = \alpha_n P_{n - 1} + \beta_{n + 1} P_{n} + \alpha_{n + 1} P_{n + 1}.
where P_n are orthonormal.

The recurrence relation for the orthonormal family can be obtained using the following correspondence,
\alpha_n = \sqrt{c_{n + 1} \over a_{n + 1} a_n}.
\beta_n = {-b_n \over a_n}.
"
  (cond
    ((<= 1 n) (* -1 (funcall #i(p.bn) n) (/ (funcall #i(p.an) n))))
    (t (error "beta-n only defined for n >= 1"))))

(defun norm-square (n p &optional (v0 #i(p.v0)))
  "(P_n, P_n)_w"
  (with-memoization ((or (slot-value p 'vn)
			 (setf (slot-value p 'vn) (make-hash-table))))
    (memoizing
     (labels ((vn (n) (cond
			((= n 0) 1)
			((< 0 n) (/ (* (funcall #i(p.cn) (1+ n)) (funcall #i(p.an) n) (vn (1- n)))
				    (funcall #i(p.an) (1+ n)))))))
       (* (vn n) v0)))))
;;
(defun gauss-quadrature (n p)
  "Computes knot points and quadrature weights by diagonalizing the Jacobi operator associated with the orthogonal polynomial [1,2].

[1] Golub, Gene H., and John H. Welsch. \"Calculation of Gauss quadrature rules.\" Mathematics of computation 23.106 (1969): 221-230.
[2] Srinivasan, Akshay. \"Spectral Methods: Applications to Quantum Mechanics and Flow Stability.\" B.Tech thesis, NITK, Surathkal
"
  (let ((p (if (typep p 'orthogonal-polynomial) p (get-orthogonal-polynomial p)))
	(jacobi (zeros (list n n))))
    (iter (for i from 0 below n)
	  (setf (ref jacobi i i) (beta-n (1+ i) p))
	  (when (> i 0) (setf (ref jacobi (1- i) i) (alpha-n i p))))
    (letv* ((s v (eig jacobi :v :u))
	    (w (t:.* #i(p.v0) (t:expt #i(v[0, :] ./ p.p0) 2))))
      (values s w))))

#+weyl
(defun orthogonal-polynomial-evaluate (n variable p)
  (with-memoization ()
    (memoizing
     (labels ((on (n x)
		(match n
		  ((< 0) 0)
		  (0 #i(p.p0))
		  ((> 0) (weyl:+ (weyl:* (weyl:+ #i(p.bn(n)) (weyl:* #i(p.an(n)) x)) (on (- n 1) x))
				 (weyl:* -1 #i(p.cn(n)) (on (- n 2) x)))))))
       (weyl:coerce (on n variable) (weyl:get-polynomial-ring (weyl:get-rational-numbers) (list variable)))))))

#+nil
(defun orthopoly (x n orp &optional (grad? 0))
  (let* ((dp (iter (for i from 0 to grad?)
		   (collect (case i
			      (0 (list #i(orp.p0 .* ones(dimensions(x))) #i(orp.p0 .* (orp.an(1) .* x + orp.bn(1)))))
			      (1 (list (zeros (dimensions x)) #i(orp.p0 .* orp.an(1) .* ones(dimensions(x)))))
			      (t (list (zeros (dimensions x)) (zeros (dimensions x)))))))))
    (iter (for j from 1 below n)
	  (let ((an #i(orp.an(j + 1))) (bn #i(orp.bn(j + 1))) (cn #i(orp.cn(j + 1))))
	    (iter (for i from 0 to grad?)
		  (for dpi in dp) (for dpp previous dpi)
		  (let ((dpi+ #i(dpi[1] .* (an .* x + bn) + \ (if (> i 0) #i(i * an * dpp[0]) 0) - cn .* dpi[0])))
		    #i(dpi[0][] = dpi[1], dpi[1][] = \ dpi+)))))
    (values-list (mapcar (if (= n 0) #'first #'second) dp))))

#+nil
(defun lagrange-derivative-matrix (x)
  (let* ((nk (dimensions x 0))
	 (ret (zeros (list nk nk) (tensor 'double-float))))
    ;; (ref ret i i) (sum k (if (= k i) 0 (/ 1 (- (ref x i) (ref x k)))))
    ;; (ref ret i j) (prod k (/ (if (or (= k i) (= k j)) 1 (- (ref x i) (ref x j)))
    ;;			     (if (= k i) 1 (- (ref x i) (ref x j)))))
    (dorefs (idx (dimensions ret))
      ((ref ret :type #.(tensor 'double-float)))
      (let ((n (aref idx 0)) (i (aref idx 1)))
	(setf ref (if (= n i)
		      (loop :for k from 0 :below nk
			 :sum (if (= k i) 0 (/ 1 (- (ref x i) (ref x k))))
			 )
		      (iter (for k from 0 below nk)
			    (summing ))
		      (iter (for j from 0 below nk)
			    (multiply (/ (if (or (= j n) (= j i)) 1 (- (ref x n) (ref x j)))
					 (if (= j i) 1 (- (ref x i) (ref x j))))))))))
    ret))
