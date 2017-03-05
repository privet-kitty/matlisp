(in-package #:matlisp)

(defun range (start end &optional h_ list-outputp &aux (h (or h_ 1)))
  (declare (type real start end h))
  (let ((quo (ceiling (if (> start end) (- start end) (- end start)) h))
	(h (if (> start end) (- h) h)))
    (if (= quo 0) nil
	(if (not list-outputp)
	    (let* ((type (realtype-max (list h start end (+ h start) (- end h)))))
	      (map-tensor! (let ((ori (coerce start type)) (h (coerce h type)))
			     (lambda (idx y) (declare (ignore idx y)) (prog1 ori (incf ori h))))
		  nil (zeros quo (tensor type))))
	    (loop :for i :from 0 :below quo
	       :for ori := start :then (+ ori h)
	       :collect ori)))))

(defun linspace (start end &optional num-points list-outputp)
  (let* ((num-points (floor (or num-points (1+ (abs (- start end))))))
	 (h (/ (- end start) (1- num-points))))
    (range start (+ h end) (abs h) list-outputp)))

;;This will only work if type is a dense-tensor
(defun ones (dims &optional (type *default-tensor-type*))
  (the dense-tensor (zeros dims type 1)))

(defun eye! (tensor)
  (tricopy! 1 (copy! 0 tensor) :d))

(defun eye (dims &optional (type *default-tensor-type*))
  (tricopy! 1 (zeros dims type) :d))
;;
(defun diagonal~ (a &optional bias)
  (declare (type dense-tensor a))
  (letv* ((off dim
	       (if bias
		   (let ((bias (etypecase bias
				 (index-type (idxv 0 bias))
				 (list (coerce bias 'index-store-vector))
				 (vector (make-array (length bias) :element-type 'index-type :initial-contents bias)))))
		     (assert (= (length bias) (order a)) nil 'tensor-index-rank-mismatch)
		     (letv* ((min (lvec-min bias)))
		       (iter (for di in-vector (dimensions a) with-index i)
			     (decf (aref bias i) min)
			     (minimizing (- di (aref bias i)) into dim)
			     (summing (* (strides a i) (aref bias i)) into off)
			     (finally
			      (assert (< 0 dim) nil 'tensor-dimension-mismatch)
			      (return (values (+ (head a) off) dim))))))
		   (values (head a) (lvec-min (dimensions a))))))
    (with-no-init-checks
	(make-instance (class-of a)
		       :dimensions (coerce (list dim) 'index-store-vector)
		       :strides (coerce (list (lvec-foldr #'+ (strides a))) 'index-store-vector)
		       :head off :store (store a) :parent a))))
(defun (setf diagonal~) (value tensor &optional bias) (copy! value (diagonal~ tensor bias)))

(defun tr (mat) (t:sum (diagonal~ mat)))
;;
(defun diag (tensor &optional (order 2))
  (declare (type (and tensor-vector dense-tensor) tensor))
  (tricopy! tensor (zeros (make-list order :initial-element (dimensions tensor 0)) (type-of tensor)) :d))

;;
(defun meshgrid (a b)
  (declare (type tensor-vector a b))
  (values (ger 1 a (ones (dimensions b 0) (class-of b)) nil)
	  (ger 1 (ones (dimensions a 0) (class-of a)) b nil)))

(defun proj-psd (m)
  (letv* ((λλ u (eig (scal! 1/2 (axpy! 1 (transpose~ m) (copy m))) :v))
	  (ret (zeros (dimensions m) (type-of m))))
    (iter (for (λi ui) slicing (list λλ u) along (list 0 -1))
	  (if (< 0 (ref λi 0)) (ger! (ref λi 0) ui ui ret t)))
    ret))

(defmacro with-coordinates ((&rest syms) vector &body code)
  (with-gensyms (vec)
    `(let ((,vec ,vector))
       (declare (type tensor-vector ,vec))
       (assert (= (dimensions ,vec 0) ,(length syms)) nil 'tensor-dimension-mismatch)
       (symbol-macrolet (,@(mapcar (let ((i -1)) #'(lambda (x) `(,x (ref ,vec ,(incf i))))) syms))
	 ,@code))))
