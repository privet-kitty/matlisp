(in-package #:matlisp)

(defun polyfit (observations &optional n &aux (observations (coerce observations 'vector)))  
  (let* ((n (or n (1- (length observations))))
	 (A (zeros (list (length observations) (1+ n))))
	 (b (zeros (dimensions A 0))))
    (labels ((coeff (n k)
	       (if (< n k) 0 (iter (for jj from n downto (- n k -1)) (multiplying jj))))
	     (row-ti (ti x &optional (derivative 0) &aux (pti 1d0))
	       (iter (for i from 0 below (dimensions x 0))
		     (setf (ref x i) (* (coeff i derivative) pti))
		     (if (<= derivative i) (setf pti (* pti ti))))))
      (iter (for li in-vector observations)
	    (for (Ai bi) slicing (list A b) along 0)
	    (ematch li
	      ((λlist ti value &optional (derivative 0))
	       (setf (ref bi 0) value)
	       (row-ti ti Ai derivative))))
      (lstsq A b))))

(defun polyval (tt poly &aux (ret 0))
  (iter (for ii from (1- (dimensions poly 0)) downto 0) ;;horner
	(setf ret (+ (* tt ret) (ref poly ii))))
  ret)

(defun roots (poly &aux (n (1- (dimensions poly 0))))
  ;;TODO: Add a better method.
  (let ((A (zeros (list n n) (type-of poly))))
    (if (< 1 n) (copy! 1 (diagonal~ A 1)))
    (scal! (/ -1 (ref poly -1)) (copy! (subtensor~ poly '((0 -1))) (subtensor~ A '(-1 (nil nil)))))
    (eig A :nn)))
