(in-package #:matlisp)

(defun gnp (n p)
  "(GNP n p)
  Samples from a Erdos-Renyi distribution of @arg{n} vertex graphs, whose edges are sampled with probability @arg{p}.

  [1] Batagelj, V., & Brandes, U. (2005). Efficient generation of large random networks. Physical Review E, 71(3), 036113."
  (assert (< 0 p 1) nil "p ∉ [0, 1]")
  (let ((ret (zeros (list n n) (tensor 'index-type 'hash-tensor)))
	(v 0) (w -1))
    (iter (while (< v n))
      (letv* ((r (random 1d0)))
	(incf w (1+ (floor (/ (log (- 1 r)) (log (- 1 p))))))       
	(iter (while (and (<= v w) (< v n)))
	      (decf w v) (incf v))
	(if (< v n)
	    (setf (ref ret v w) 1
		  (ref ret w v) 1))))
    (copy ret (tensor 'index-type 'simple-graph-tensor))))

(defun gnm (n m &aux (max-m (* n (- n 1) 1/2)))
  "(GNM n m)
  Samples uniformly from the set of @arg{n} vertex graphs with @arg{m} edges.

  [1] Batagelj, V., & Brandes, U. (2005). Efficient generation of large random networks. Physical Review E, 71(3), 036113."
  (assert (< 0 m max-m) nil "p ∉ (0, (n 2))")
  (let ((ret (zeros (list n n) (tensor 'index-type 'hash-tensor))))
    (iter (for ii below m)
	  (iter 
	    (letv* ((v w (floor (random (* n n)) n)))
	      (when (and (/= v w) (not (nth-value 1 (ref ret v w))))
		(setf (ref ret v w) 1
		      (ref ret w v) 1)
		(finish)))))
    (copy ret (tensor 'index-type 'simple-graph-tensor))))
