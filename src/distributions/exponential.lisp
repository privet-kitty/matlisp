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

(definline exponential-function (x) (exp (- x)))
(definline exponential-tail-sampler (r0 f0 identity)
  (let* ((u1 (- 1 (random identity))))
    (+ r0 (* -1 (log u1)))))

(declaim (ftype (function () double-float) random-exponential))
(eval-when (:compile-toplevel)
  (letv* ((points v (ziggurat-bisect #'exponential-function 15d0 :n-divisions (expt 2 8) :x_0-atol 1d-14))
	  (points (cons 0d0 (cdr points))))
    (setf (symbol-function 'random-exponential)
	  (compile-and-eval
	   (ziggurat-compile #'exponential-function points v
			     #'random #'random #'exponential-tail-sampler
			     :symmetricp nil)))))
