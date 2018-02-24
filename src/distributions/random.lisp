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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-documentation-body (body)
    (match body
      ((list* (and (type string) documentation) body) (values body (list documentation)))
      (body body)))

  (defun merge-lambda-lists (args-1 args-2)
    (let* ((args-1 (trivia.level2.impl::parse-lambda-list args-1))
	   (args-2 (trivia.level2.impl::parse-lambda-list args-2))
	   (ret (mapcar #'(lambda (k) (append (or (assoc k args-1) (list k)) (cdr (assoc k args-2))))
			'(:atom :optional :keyword))))
      `(,@(cdr (assoc :atom ret))
	  ,@(if-let (args (cdr (assoc :optional ret)))
	      `(&optional ,@args))
	  ,@(if-let (args (cdr (assoc :keyword ret)))
	      `(&key ,@args))))))

(defmacro defrandom (name args &body body)
  (letv* ((body documentation (parse-documentation-body body)))
    (with-gensyms (ret ref-ret)
      `(definline ,name (,@(merge-lambda-lists '( &optional shape) args))
	 ,@documentation
	 (if (not shape) (let () ,@body)
	     (let ((,ret (zeros shape '#.(tensor 'double-float))))
	       (dorefs (_ (dimensions ,ret))
		 ((,ref-ret ,ret :type #.(tensor 'double-float)))
		 (setf ,ref-ret (let () ,@body)))
	       ,ret))))))

;; NOTE: all of the samplers are built using random-byte-kernel
(definline random-byte-kernel (arg)
  "
  Random sample from the Uniform distribution on {0, arg-1}
  "
  (random (the unsigned-byte arg)))

;;
(defrandom random-uniform (&optional (zero-openp nil))
  "
  Random sample from the Uniform distribution on [0, 1);
  if ZERO-OPENP then from (0, 1].
  "
  (if zero-openp
      (- 1 (scale-float (float (random-byte-kernel #.(expt 2 52)) 1d0) -52))
      (scale-float (float (random-byte-kernel #.(expt 2 52)) 1d0) -52)))

;; pareto
(defrandom random-pareto (a b)
  "
  Random value for parato distribution:
  p(x) dx = (a/b) / (x/b)^(a+1) dx     for x >= b
"
  (let* ((a (coerce a 'double-float))
	 (x (random-uniform nil t))
	 (z (expt x (/ -1d0 a))))
    (declare (type double-float a x z))
    (* (coerce b 'double-float) z)))
