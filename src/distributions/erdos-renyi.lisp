;; Copyright (c) 2018 Akshay Srinivasan <akshaysrinivasan@gmail.com>

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

(defun random-gnp (n p)
  "(RANDOM-GNP n p)
  Samples from a Erdos-Renyi distribution of @arg{n} vertex graphs, whose edges are sampled with probability @arg{p}.

  [1] Batagelj, V., & Brandes, U. (2005). Efficient generation of large random networks. Physical Review E, 71(3), 036113."
  (assert (< 0 p 1) nil "p ∉ [0, 1]")
  (let ((ret (zeros (list n n) (tensor 'index-type 'hash-tensor)))
	(v 0) (w -1))
    (iter (while (< v n))
      (letv* ((r (random-uniform)))
	(incf w (1+ (floor (/ (log (- 1 r)) (log (- 1 p))))))       
	(iter (while (and (<= v w) (< v n)))
	      (decf w v) (incf v))
	(if (< v n)
	    (setf (ref ret v w) 1
		  (ref ret w v) 1))))
    (copy ret (tensor 'index-type 'simple-graph-tensor))))

(defun random-gnm (n m &aux (max-m (* n (- n 1) 1/2)))
  "(RANDOM-GNM n m)
  Samples uniformly from the set of @arg{n} vertex graphs with @arg{m} edges.

  [1] Batagelj, V., & Brandes, U. (2005). Efficient generation of large random networks. Physical Review E, 71(3), 036113."
  (assert (< 0 m max-m) nil "p ∉ (0, (n 2))")
  (let ((ret (zeros (list n n) (tensor 'index-type 'hash-tensor))))
    (iter (for ii below m)
	  (iter 
	    (letv* ((v w (floor (random-byte-kernel (* n n)) n)))
	      (when (and (/= v w) (not (nth-value 1 (ref ret v w))))
		(setf (ref ret v w) 1
		      (ref ret w v) 1)
		(finish)))))
    (copy ret (tensor 'index-type 'simple-graph-tensor))))
