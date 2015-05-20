(in-package #:matlisp)

;;
(defgeneric subtensor~ (tensor subscripts)
  (:documentation "
  Syntax
  ======
  (SUBTENSOR~ TENSOR SUBSCRIPTS &optional PRESERVE-RANK REF-SINGLE-ELEMENT?)

  Purpose
  =======
  Creates a new tensor data structure, sharing store with
  TENSOR but with different strides and dimensions, as defined
  in the subscript-list SUBSCRIPTS.

  Examples
  ========
  > (defvar X (make-real-tensor 10 10 10))
  X

  ;; Get (:, 0, 0)
  > (subtensor~ X '((nil nil . nil) (0 1 . nil) (0 1 . nil)))

  ;; Get (:, 2:5, :)
  > (subtensor~ X '((nil nil . nil) (2 5 . nil)))

  ;; Get (:, :, 0:2:10) (0:10:2 = [i : 0 <= i < 10, i % 2 = 0])
  > (subtensor~ X '((nil nil . nil) (nil nil . nil) (0 10 . 2)))

  Commentary
  ==========
  Sadly in our parentheses filled world, this function has to be necessarily
  verbose (unlike MATLAB, Python). However, this function has been designed with the
  express purpose of using it with a Lisp reader macro. The slicing semantics is
  essentially the same as MATLAB except for the zero-based indexing.
")
  (:method :before ((tensor tensor) (subscripts list))
     (assert (or (null subscripts) (= (length subscripts) (order tensor))) nil 'tensor-index-rank-mismatch)))

(defgeneric (setf subtensor~) (value tensor subscripts)
  (:method :before (value (tensor tensor) (subscripts list))
     (assert (or (null subscripts) (= (length subscripts) (order tensor))) nil 'tensor-index-rank-mismatch)))

(definline slice~ (x axis &optional (idx 0) (preserve-rank? (when (= (order x) 1) t)))
  (subtensor~ x
	      (iter (for i from 0 below (order x))
		    (with axis = (modproj axis (order x) nil 0))
		    (collect (cond ((/= i axis) '(nil nil))
				   (preserve-rank? (list idx (1+ idx)))
				   (t idx))))))
;;Helper functions
(definline parse-slice (subs dimensions)
  (declare (type index-store-vector dimensions))
  (iter (for sub.i in subs)
	(for d in-vector dimensions) (declare (type index-type d))
	(if (not (consp sub.i))
	    (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
	      (collect 1 into dims)
	      (collect idx into psubs))
	    (destructuring-bind (start end . inc) sub.i
	      (declare ((or index-type null) start end inc))
	      (let* ((inc (modproj inc nil nil 1))
		     (start (modproj start d nil (if (> inc 0) 0 (1- d))))
		     (end (modproj end d t (if (> inc 0) d -1)))
		     (nd (ceiling (- end start) inc)))
		(declare (type index-type start end inc nd))
		(when (<= nd 0) (return nil))
		(collect nd into dims)
		(collect (list* start end inc) into psubs))))
	(finally (return (values psubs dims)))))

(definline parse-slice-for-strides (subscripts dimensions strides)
  (declare (type index-store-vector dimensions strides)
	   (type list subscripts))
  (iter (for sub.i in subscripts)
	(for d in-vector dimensions)
	(for s in-vector strides)
	(with (the index-type hd) = 0)
	(if (not (consp sub.i))
	    (let ((idx (modproj (the (or index-type null) sub.i) d nil 0)))
	      (incf hd (* s idx)))
	    (destructuring-bind (start end . inc) sub.i
	      (declare ((or index-type null) start end inc))
	      (let* ((inc (modproj inc nil nil 1))
		     (start (modproj start d nil (if (> inc 0) 0 (1- d))))
		     (end (modproj end d t (if (> inc 0) d -1)))
		     (nd (ceiling (- end start) inc)))
		(declare (type index-type start end inc nd))
		(when (<= nd 0) (return nil))
		(incf hd (* s start))
		(collect nd into dims)
		(collect (* inc s) into stds))))
	(finally (return (values hd dims stds)))))
;;
(defmethod subtensor~ ((tensor dense-tensor) (subscripts list))
  (letv* ((hd dims stds (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))))
    (cond
      ((not hd) nil)
      ((not dims) (if subscripts
		      (store-ref tensor hd)
		      (with-no-init-checks
			  (make-instance (class-of tensor)
					 :head (head tensor)
					 :dimensions (copy-seq (dimensions tensor))
					 :strides (copy-seq (strides tensor))
					 :store (slot-value tensor 'store)
					 :parent tensor))))
      (t (with-no-init-checks
	     (make-instance (class-of tensor)
			    :head (+ hd (head tensor))
			    :dimensions (coerce dims 'index-store-vector)
			    :strides (coerce stds 'index-store-vector)
			    :store (slot-value tensor 'store)
			    :parent tensor))))))

(defmethod (setf subtensor~) (value (tensor dense-tensor) (subscripts list))
  (letv* ((hd dims stds (parse-slice-for-strides subscripts (dimensions tensor) (strides tensor))))
    (cond
      ((not hd) nil #+nil(error "no place found inside ~a." subscripts))
      ((not dims) (if subscripts
		      (setf (store-ref tensor hd) value)
		      (copy! value (with-no-init-checks (subtensor~ tensor nil)))))
      (t (copy! value
		(with-no-init-checks
		    (make-instance (class-of tensor)
				   :head (+ hd (head tensor))
				   :dimensions (coerce dims 'index-store-vector)
				   :strides (coerce stds 'index-store-vector)
				   :store (slot-value tensor 'store)
				   :parent tensor)))))))
;;
(defgeneric suptensor~ (tensor ord &optional start)
  (:method :before ((tensor base-tensor) ord &optional (start 0))
     (declare (type index-type start))
     (assert (<= 0 start (- ord (order tensor))) nil 'invalid-arguments)))

(defmethod suptensor~ ((ten dense-tensor) ord &optional (start 0))
  (declare (type index-type ord start))
  (if (= (order ten) ord) ten
      (with-no-init-checks
	  (make-instance (class-of ten)
			 :dimensions (coerce (nconc (make-list start :initial-element 1)
						    (lvec->list (dimensions ten))
						    (make-list (- ord (order ten) start) :initial-element 1))
					     'index-store-vector)
			 :strides (coerce (nconc (make-list start :initial-element (total-size ten))
						 (lvec->list (strides ten))
						 (make-list (- ord (order ten) start) :initial-element (total-size ten)))
					  'index-store-vector)
			 :head (head ten) :store (slot-value ten 'store) :parent ten))))
;;
(defgeneric reshape! (tensor dims)
  (:documentation "
  (RESHAPE! tensor dims)
  Reshapes the @arg{tensor} to the shape in @arg{dims}.

  This function expects all the strides to be of the same sign when
  @arg{tensor} is subtype of dense-tensor.")
  (:method :before ((tensor dense-tensor) (dims cons))
	   (assert (iter (for s in-vector (strides tensor))
			 (unless (> (* s (strides tensor 0)) 0) (return nil))
			 (finally (return t)))
		   nil 'tensor-error :message "strides are not of the same sign." :tensor tensor)
	   (assert (<= (iter (for i in dims) (multiplying i)) (total-size tensor)) nil 'tensor-insufficient-store)))

(definline matrixify~ (vec &optional (col-vector? t))
  (if (tensor-matrixp vec) vec (suptensor~ vec 2 (if col-vector? 0 1))))

(defmethod reshape! ((ten dense-tensor) (dims cons))
  (let ((idim (coerce dims 'index-store-vector)))
    (setf (slot-value ten 'dimensions) idim
	  (slot-value ten 'strides) (let ((strd (make-stride idim)))
				      (when (< (strides ten 0) 0)
					(iter (for i from 0 below (length strd))
					      (setf (aref strd i) (- (aref strd i)))))
				      strd))
    ten))

;;
(defun tensor-append (axis tensor &rest more-tensors)
  (if (null tensor)
      (when more-tensors
	(apply #'tensor-append axis (car more-tensors) (cdr more-tensors)))
      (let ((dims (copy-seq (dimensions tensor))))
	(iter (for ele in more-tensors) (incf (aref dims axis) (aref (dimensions ele) axis)))
	(let* ((ret (zeros dims (class-of tensor)))
	       (view (slice~ ret axis 0 t)))
	  (iter (for ele in (cons tensor more-tensors))
		(with head = 0)
		(setf (slot-value view 'head) head
		      (aref (dimensions view) axis) (aref (dimensions ele) axis))
		(copy! ele view)
		(incf head (* (aref (strides ret) axis) (aref (dimensions ele) axis))))
	  ret))))
;;
(defgeneric minors (x &rest indices)
  (:documentation "Copy minors of x corresponding to indices."))

(define-tensor-method minors ((x dense-tensor :x) &rest indices)
  `(letv* ((inc-x stable (minors-strides-precompute (lvec->list (dimensions x)) (lvec->list (strides x)) indices) :type index-type simple-vector)
	   (sto-x (store x) :type ,(store-type (cl x)))
	   (y (zeros (iter (for ss in-vector stable) (collect (length ss))) ',(cl x)) :type ,(cl x))
	   (sto-y (store y) :type ,(store-type (cl x))))
     (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))
								   (:minor ((of-x stable (+ inc-x (head x)))))))
	   (setf (t/store-ref ,(cl x) sto-y of-y) (t/store-ref ,(cl x) sto-x of-x)))
     y))

(defgeneric (setf minors) (value x &rest indices)
  (:documentation "Copy y onto the minors of x corresponding w indices."))

(define-tensor-method (setf minors) ((y dense-tensor :y) (x dense-tensor :x t) &rest indices)
  `(letv* ((inc-x stable (minors-strides-precompute (lvec->list (dimensions x)) (lvec->list (strides x)) indices) :type index-type simple-vector)
	   (sto-x (store x) :type ,(store-type (cl x)))
	   (sto-y (store y) :type ,(store-type (cl y))))
     (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))
								   (:minor ((of-x stable (+ inc-x (head x)))))))
	   (setf (t/store-ref ,(cl x) sto-x of-x) (t/strict-coerce (,(field-type (cl y)) ,(field-type (cl x))) (t/store-ref ,(cl y) sto-y of-y))))
     x))
