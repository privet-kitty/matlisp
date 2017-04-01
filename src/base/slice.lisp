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
  > (defvar X (zeros '(10 10 10)))
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
(closer-mop:defmethod subtensor~ ((tensor dense-tensor) (subscripts list))
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

(closer-mop:defmethod (setf subtensor~) (value (tensor dense-tensor) (subscripts list))
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

(closer-mop:defmethod suptensor~ ((ten dense-tensor) ord &optional (start 0))
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

(closer-mop:defmethod reshape! ((ten dense-tensor) (dims cons))
  (let ((idim (coerce dims 'index-store-vector)))
    (setf (slot-value ten 'dimensions) idim
	  (slot-value ten 'strides) (let ((strd (make-stride idim)))
				      (when (< (strides ten 0) 0)
					(iter (for i from 0 below (length strd))
					      (setf (aref strd i) (- (aref strd i)))))
				      strd))
    ten))

(defun reshape~ (x dims) (reshape! (subtensor~ x nil) dims))

;;
(defun join (axis tensor &rest more-tensors)
  (if (null tensor)
      (when more-tensors (apply #'join (list* axis more-tensors)))
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
(closer-mop:defgeneric minors (x &rest indices)
  (:documentation "Copy minors of x corresponding to indices.")
  (:generic-function-class tensor-method-generator))

(with-memoization (#+sbcl (trivial-garbage:make-weak-hash-table :weakness :key-or-value :test 'equalp)
		   #-sbcl (make-hash-table :test 'equalp))
  (memoizing
   (defun minors-strides-precompute (dims std indices)
     (declare (type index-store-vector dims std))
     (macrolet ((kernel (jj) `(the index-type (* (aref std ii) (modproj (the index-type ,jj) (aref dims ii))))))
       (iter (for idx in indices)
	     (let-typed ((sv (t/store-allocator index-store-vector (length idx) :initial-element 0) :type index-store-vector))
	       (iter (for i from 0 below (length sv))
		     (for jj in idx) (declare (type index-type i jj))
		     (setf (aref sv i) (kernel jj)))
	       (collect sv into stable result-type simple-vector))
	     (declare (type index-type ii))
	     (counting t into ii)
	     (finally (return stable)))))))

;;This is fairly useless.
(defun minors-check (dims-y stable)
  (declare (type index-store-vector dims-y)
	   (type vector stable))
  (assert (= (length dims-y) (length stable)) nil 'tensor-index-rank-mismatch)
  (assert (loop :for i :of-type index-type :from 0 :below (length stable)
	     :do (unless (= (aref dims-y i) (length (aref stable i))) (return nil))
	     :finally (return t)) nil 'tensor-dimension-mismatch))

(closer-mop:defmethod for-mod-iterator ((clause-name (eql :minor)) init dims minors)
  (binding-gensyms (gm gf)
    (list `(,@(mapcan #'(lambda (x) `((with ,(first x) = (or ,(third x) 0))
				      (with ,(gf (first x)) = ,(second x))))
		      minors)
	      (initially ,@(mapcar #'(lambda (x) `(iter (for ,(gm ii) in-vector ,(gf (first x)))
							(declare (type index-store-vector ,(gm ii)))
							(incf ,(first x) (aref ,(gm ii) 0))))
				   minors))
	      (declare (type simple-vector ,@(mapcar #'(lambda (x) (gf (first x))) minors))
		       (type index-type ,@(mapcar #'first minors))))
	  `(:update (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
		    (declare (ignore ,(gm init) ,(gm dims)))
		    (let-typed ((,(gm ii) (aref ,(gm idx) ,(gm count)) :type index-type))
		      ,@(mapcar #'(lambda (x)
				    `(let-typed ((,(gm vv) (aref ,(gf (first x)) ,(gm count)) :type index-store-vector))
				       (incf ,(first x) (- (aref ,(gm vv) (1+ ,(gm ii))) (aref ,(gm vv) ,(gm ii)))))) minors)))
	  `(:reset (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
		   (declare (ignore ,(gm init) ,(gm dims)))
		   (let-typed ((,(gm ii) (aref ,(gm idx) ,(gm count)) :type index-type))
		     ,@(mapcar #'(lambda (x)
				   `(let-typed ((,(gm vv) (aref ,(gf (first x)) ,(gm count)) :type index-store-vector))
				      (incf ,(first x) (- (aref ,(gm vv) 0) (aref ,(gm vv) ,(gm ii)))))) minors))))))
;;
(define-tensor-method minors ((x stride-tensor :x) &rest indices)
  (let ((ret-class (if (subtypep (cl :x) 'dense-tensor) (cl :x) (tensor (field-type (cl :x))))))
    `(letv* ((stable (minors-strides-precompute (dimensions x) (strides x) indices) :type simple-vector)
	     (sto-x (store x) :type ,(store-type (cl :x)))
	     (y (zeros (iter (for ss in-vector stable) (collect (length ss))) ',ret-class) :type ,ret-class)
	     (sto-y (store y) :type ,(store-type ret-class)))
       (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))
								     (:minor ((of-x stable (head x))))))
	     (setf (t/store-ref ,ret-class sto-y of-y) (t/store-ref ,(cl :x) sto-x of-x)))
       y)))

(define-tensor-method minors ((x tensor :x) &rest indices)
  (let ((ret-class (tensor (field-type (cl :x)))))
    `(letv* ((stable (map 'vector #'(lambda (x) (coerce x 'index-store-vector)) indices) :type simple-vector)
	     (y (zeros (map 'list #'length stable) ',ret-class) :type ,ret-class)
	     (sto-y (store y) :type ,(store-type ret-class))
	     (sidx (t/store-allocator index-store-vector (length stable))))
       (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))))
	     (iter (for i from 0 below (length sidx)) (setf (aref sidx i) (aref (aref stable i) (aref idx i))))
	     (setf (t/store-ref ,ret-class sto-y of-y) (ref x sidx)))
       y)))

(closer-mop:defgeneric (setf minors) (value x &rest indices)
  (:documentation "Copy y onto the minors of x corresponding w indices.")
  (:generic-function-class tensor-method-generator))

(define-tensor-method (setf minors) ((y dense-tensor :y) (x stride-tensor :x t) &rest indices)
  `(with-memoization ()
     (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))
								   (:minor ((of-x (minors-strides-precompute (dimensions x) (strides x) indices) (head x))))))
	   (setf (t/store-ref ,(cl :x) (memoizing (store x) :type ,(store-type (cl :x)) :global t) of-x)
		 (t/strict-coerce (,(field-type (cl :y)) ,(field-type (cl :x)))
				  (t/store-ref ,(cl :y) (memoizing (store y) :type ,(store-type (cl :y)) :global t) of-y))))
     x))

(define-tensor-method (setf minors) ((y t) (x stride-tensor :x t) &rest indices)
  `(with-memoization ()
     (letv* ((stable (minors-strides-precompute (dimensions x) (strides x) indices) :type simple-vector))
       (iter (for-mod idx from 0 below (map 'index-store-vector #'length stable) with-iterator ((:minor ((of-x stable (head x))))))
	     (setf (t/store-ref ,(cl :x) (memoizing (store x) :type ,(store-type (cl :x)) :global t) of-x)
		   (memoizing (t/coerce ,(field-type (cl :x)) y) :type ,(field-type (cl :x)) :global t)))
       x)))

(define-tensor-method (setf minors) ((y dense-tensor :y) (x stride-tensor :x t) &rest indices)
  `(letv* ((stable (minors-strides-precompute (dimensions x) (strides x) indices) :type simple-vector)
	   (sto-x (store x) :type ,(store-type (cl :x)))
	   (sto-y (store y) :type ,(store-type (cl :y))))
     (iter (for-mod idx from 0 below (dimensions y) with-iterator ((:stride ((of-y (strides y) (head y))))
								   (:minor ((of-x stable (head x))))))
	   (setf (t/store-ref ,(cl :x) sto-x of-x) (t/strict-coerce (,(field-type (cl :y)) ,(field-type (cl :x))) (t/store-ref ,(cl :y) sto-y of-y))))
     x))
;;

