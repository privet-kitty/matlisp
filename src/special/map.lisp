(in-package #:matlisp)

(closer-mop:defgeneric map-tensor! (func x y)
  (:documentation
"
    Syntax
    ======
    (MAP-TENSOR! func x y)

    Purpose
    =======
    Applies the function element-wise on x, and sets the corresponding
    elements in y to the value returned by the function.

    Example
    =======
    > (map-tensor! #'(lambda (idx x y)
		  (if (= (car idx) (cadr idx))
		      (sin x)
		      y))
       (randn '(2 2)) (zeros '(2 2)))
    #<REAL-TENSOR #(2 2)
    -9.78972E-2  0.0000
     0.0000     -.39243
    >
    >
")
  (:method :before ((func function) (x tensor) (y tensor))
     (assert (very-quickly (lvec-eq (dimensions x) (dimensions y))) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))
(define-tensor-method map-tensor! ((func function) (x dense-tensor :x) (y dense-tensor :y))
  `(dorefs (idx (dimensions x))
     ((ref-x x :type ,(cl :x))
      (ref-y y :type ,(cl :y)))
     (setf ref-y (funcall func idx ref-x ref-y)))
  'y)
(define-tensor-method map-tensor! ((func function) (x null) (y dense-tensor :y))
  `(dorefs (idx (dimensions y))
     ((ref-y y :type ,(cl :y)))
     (setf ref-y (funcall func idx ref-y)))
  'y)

(definline map-tensor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (map-tensor! #'(lambda (idx x y) (declare (ignore idx y)) (funcall func x)) x ret)))
;;
(defun check-dims (axlst tensors)
  (let ((axlst (if (numberp axlst) (make-list (length tensors) :initial-element axlst) axlst)))
    (iter (for x in tensors)
	  (for axis in axlst)
	  (with dims = nil)
	  (cond
	    ((typep x 'dense-tensor)
	     (let-typed ((xdims (dimensions x) :type index-store-vector))
			(assert (< axis (order x)) nil 'tensor-dimension-mismatch)
			(if (null dims)
			    (setf dims (aref xdims (mod axis (order x))))
			    (setf dims (min (aref xdims (mod axis (order x))) dims))))
	     (collect (aref (strides x) (mod axis (order x))) into strides)
	     (collect (slice~ x axis 0 (if (> (order x) 1) nil t)) into slices))
	    ((eq x nil)
	     (collect nil into strides)
	     (collect nil into slices))
	    (t (error 'invalid-arguments)))
	  (finally (return (values dims strides slices))))))

(defun mapslice~ (axis func tensor &rest more-tensors)
  (letv* ((d.axis strides slices (check-dims axis (cons tensor more-tensors))))
    (loop :for i :from 0 :below d.axis
       :collect (prog1 (apply func slices)
		  (when (< i (1- d.axis))
		    (loop :for slc :in slices
		       :for std :in strides
		       :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslicec~ (axis func tensor &rest more-tensors)
  (apply #'mapslice~ axis func tensor more-tensors)
  (values-list (list* tensor more-tensors)))

(defun mapslice (axis func tensor &rest more-tensors)
  (apply #'mapslice~ axis #'(lambda (&rest args) (apply func (mapcar #'copy args))) tensor more-tensors))
;;
(defmacro fold-tensor (index (tensor initial) &rest body &environment env
		       &aux (index (or index (gensym "INDEX"))))
  (letv* ((((ref tensor tensor-type)
	    (ret ret-0 ret-type))
	   (iter (for x in (list tensor initial))
		 (ematch x
		   ((λlist name expr &key type)
		    (collect (list name expr (or type (infer-type expr env)))))))))
    (using-gensyms (decl (tensor) (out))
      `(let* (,@decl
	      (,ret ,ret-0))
	 (declare (type ,tensor-type ,tensor )
		  (type ,ret-type ,ret))
	 (very-quickly
	   (dorefs (,index (dimensions ,tensor))
		   ((,ref ,tensor :type ,tensor-type))
	     (let ((,out (progn ,@body)))
	       (if ,out (setf ,ret ,out)))))
	 ,ret))))
;;
(defmacro-clause (FOR xa in-vectors x &optional FROM start BELOW oend TO cend DOWNTO dend WITH-INDEX index)
  (let ((syms (zipsym (ensure-list xa))))
    (with-gensyms (xeval)
      `(progn
	 ,@(mapcar #'(lambda (x) `(with ,(car x) = nil)) syms)
	 (initially (let ((,xeval ,x))
		      (setf ,@(mapcan #'(lambda (x) `(,(car x) (car ,xeval)
						       ,xeval (cdr ,xeval))) syms))))
	 ,@(mapcar (let ((first? t))
		     #'(lambda (x) `(for ,(cadr x) in-vector ,(car x) FROM ,start BELOW ,oend TO ,cend DOWNTO ,dend
				    ,@(when first? (setf first? nil) `(WITH-INDEX ,index)))))
		   syms)))))

(defmacro-clause (FOR xa in-lists x &optional BY step-function)
  (let ((syms (zipsym (ensure-list xa))))
    (with-gensyms (xeval)
      `(progn
	 ,@(mapcar #'(lambda (x) `(with ,(car x) = nil)) syms)
	 (initially (let ((,xeval ,x))
		      (setf ,@(mapcan #'(lambda (x) `(,(car x) (car ,xeval)
						  ,xeval (cdr ,xeval))) syms))))
	 ,@(mapcar #'(lambda (x) `(for ,(cadr x) in ,(car x) BY ,(or step-function #'cdr))) syms)))))
;;
(defmacro-clause (FOR xa SLICING x ALONG axis &optional FROM start BELOW oend TO cend DOWNTO dend WITH-INDEX index BY step)
  (when (or (and oend cend) (and dend (or cend oend))) (error "Use only one of BELOW TO DOWNTO."))
  (when-let (xa (ensure-list xa))
    (binding-gensyms (hy hyf)
      (let ((n (length xa)))
	`(progn
	   (with ,(hy x) = ,x)
	   (with ,(hy dim) = -1)
	   ,@(mapcan #'(lambda (x y) (when x `((with ,(hyf y) = ,x) (declare (type index-type ,(hyf y))))))
		     (list start oend cend dend step)
		     '(start oend cend dend step))
	   (with ,(hy axis) = (let ((,(hy axis) ,axis))
				(if (listp ,(hy axis))
				    (assert (= (length ,(hy axis)) ,n) nil 'invalid-arguments)
				    (setq ,(hy axis) (make-list ,n :initial-element ,(hy axis))))
				,(hy axis)))
	   ,@(mapcan #'(lambda (x) `((with ,x = nil))) xa)
	   (initially
	    (let ((,(hy x) (if (listp ,(hy x))
			       (progn (assert (= (length ,(hy x)) ,n) nil 'invalid-arguments) ,(hy x))
			       (make-list ,n :initial-element ,(hy x)))))
	      (iter (for ,(hy xi) in ,(hy x))
		    (for ,(hy as) on ,(hy axis))
		    (etypecase ,(hy xi)
		      (null (setf (car ,(hy as)) nil))
		      (dense-tensor
		       (let* ((,(hy ai) (modproj (car ,(hy as)) (order ,(hy xi))))
			      (,(hy xi) ,(if (or start oend cend dend)
					     `(let ((,(hy dimi) (dimensions ,(hy xi) (the index-type ,(hy ai))))
						    (,(hy slice) (make-list (order ,(hy xi)) :initial-element '(nil nil))))
						(declare (ignorable ,(hy dimi)))
						(setf (nth ,(hy ai) ,(hy slice))
						      ,(cond
						    (dend `(list* ,(and start (hy start)) (- (modproj ,(and dend (hy dend)) ,(hy dimi)) ,(hy dimi) 1) -1))
						    (oend `(list ,(and start (hy start)) ,(and oend (hy oend))))
						    (cend `(list ,(and start (hy start)) (1+ (modproj ,(and cend (hy cend)) ,(hy dimi)))))
						    (t `(list ,(and start (hy start)) nil))))
						(subtensor~ ,(hy xi) ,(hy slice)))
					     (hy xi))))
			 (if ,(hy xi)
			     (progn
			       (when (or (< ,(hy dim) 0) (> ,(hy dim) (dimensions ,(hy xi) (the index-type ,(hy ai)))))
				 (setf ,(hy dim) (dimensions ,(hy xi) (the index-type ,(hy ai)))))
			       (setf (car ,(hy as))
				     (cons (let ((,(hy xs) (slice~ ,(hy xi) ,(hy ai))))
					     (setf (gethash 'slice-increment (memos ,(hy xs))) (strides ,(hy xi) ,(hy ai)))
					     ,(hy xs))
					   (strides ,(hy xi) ,(hy ai)))))
			     (setf ,(hy dim) 0
				   (car ,(hy as)) (cons nil nil))))))))
	    (let ((,(hy axis) ,(hy axis)))
	      (setf ,@(mapcan #'(lambda (x) `(,x (caar ,(hy axis)) ,(hy axis) (cdr ,(hy axis)))) xa))))
	   (repeat ,(if step `(floor ,(hy dim) ,(hy step)) (hy dim)))
	   ,@(when index `((for ,index initially ,(or (and start (hy start)) (if dend `(1- ,(hy dim)) 0)) then (,(if dend '- '+) ,index ,(or (and step (hy step)) 1)))))
	   (after-each
	    (iter (for ,(hy ai) in ,(hy axis))
		  (when ,(hy ai) (incf (slot-value (car ,(hy ai)) 'head) ,(recursive-append (when step `(* ,(hy step))) `(cdr ,(hy ai))))))))))))
