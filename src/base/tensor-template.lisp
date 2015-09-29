(in-package #:matlisp)

;;
(deft/generic (t/store-type #'subtypep) sym (&optional size))
(eval-every
  (defun real-subtype (cl) (when (and (consp cl) (eql (first cl) 'complex)) (second cl)))
  (defun store-type (cl &optional (size '*)) (macroexpand-1 `(t/store-type ,cl ,size)))
  (defun linear-storep (cl) (eql (first (ensure-list (store-type cl))) 'simple-array))
  (defun hash-table-storep (x) (eql (store-type x) 'hash-table))
  (defun clinear-storep (x) (and (subtypep x 'tensor) (linear-storep x) (real-subtype (field-type x))))
  (defun float-tensorp (type) (member (field-type type) '(single-float double-float (complex single-float) (complex double-float)) :test #'equal))
  (defun tensor-leafp (x)
    (let* ((x (etypecase x (class x) (symbol (find-class x)))))
      (and
       (= (length (intersection (mapcar #'find-class '(base-tensor tensor dense-tensor)) (closer-mop:class-direct-superclasses x))) 1)
       (not (find-if #'tensor-leafp (closer-mop:class-direct-subclasses x)))))))

(deft/method t/store-type (sym graph-accessor) (&optional (size '*))
  `(simple-array ,(or (real-subtype (field-type sym)) (field-type sym)) (,size)))
(deft/method t/store-type (sym coordinate-accessor) (&optional (size '*))
  `(simple-array ,(or (real-subtype (field-type sym)) (field-type sym)) (,size)))

;;
(deft/generic (t/store #'subtypep) sym (x))
(deft/method t/store (sym tensor) (x) `(the ,(store-type sym) (slot-value ,x 'store)))

;;tensor specializations
(deft/generic (t/field-type #'subtypep) sym ())
(eval-every
  (defun field-type (clname) (macroexpand-1 `(t/field-type ,clname)))
  (defun coerceable? (clx cly)
    (handler-case (progn (macroexpand-1 `(t/strict-coerce ((t/field-type ,clx) (t/field-type ,cly)) x)) t)
      (error () nil))))

(deft/method t/field-type (sym tensor) () t)
;;This is useful for Eigenvalue decompositions
(deft/generic (t/complexified-type #'subtypep) sym ())
(eval-every (defun complexified-type (type) (macroexpand-1 `(t/complexified-type ,(typecase type (symbol type) (class (class-name type)) (t (class-name (class-of type))))))))

(deft/method t/complexified-type (sym tensor) ()
  (letv* (((type accessor &optional store) (tensor-load-form sym)))
    (cond
      ((real-subtype type) sym)
      ((subtypep type 'real) (tensor (list 'complex type) accessor store))
      (t (error "Unknown complex type for ~a" sym)))))

;;Now we're just making up names
(deft/generic (t/realified-type #'subtypep) sym ())
(eval-every (defun realified-type (type) (macroexpand-1 `(t/realified-type ,(typecase type (symbol type) (class (class-name type)) (t (class-name (class-of type))))))))

(deft/method t/realified-type (sym tensor) ()
  (letv* (((type accessor &optional store) (tensor-load-form sym)))
    (if (subtypep type 'complex)
	(if (real-subtype type)
	    (tensor (real-subtype type) accessor store)
	    (tensor 'real accessor store))
	sym)))
;;
(deft/generic (t/compute-store-size #'subtypep) sym (size))
(deft/generic (t/store-size #'subtypep) sym (ele))
(deft/generic (t/store-allocator #'subtypep) sym (size &rest initargs))
(deft/generic (t/total-size #'subtypep) sym (ele))
;;
(deft/method (t/compute-store-size #'linear-storep) (sym tensor) (size)
  (if (clinear-storep sym) `(* 2 ,size) size))

(deft/method (t/store-size #'linear-storep) (sym tensor) (ele)
  (if (clinear-storep sym) `(/ (length ,ele) 2) `(length ,ele)))

(deft/method (t/store-size #'hash-table-storep) (sym stride-accessor) (ele)
  `(hash-table-size ,ele))
;;
(deft/method t/total-size (sym dense-tensor) (ele)
  `(lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions ,ele))))

(deft/method (t/total-size #'hash-table-storep) (sym tensor) (ele)
  `(hash-table-count (t/store ,sym ,ele)))

(deft/method t/total-size (sym graph-accessor) (ele)
  `(nth-value 1 (fence ,ele -1)))

(deft/method t/total-size (sym coordinate-accessor) (ele)
  `(slot-value ,ele 'boundary))
;;
(deft/method t/store-allocator (sym index-store) (size &rest initargs)
  (letv* ((() initargs))
    `(the index-store (make-array ,size :element-type 'index-type))))
(deft/method t/store-allocator (sym index-store-vector) (size &rest initargs)
  (letv* (((&key initial-element initial-contents) initargs))
    `(the index-store-vector (make-array ,size :element-type 'index-type
					 ,@(when initial-element `(:initial-element ,initial-element))
					 ,@(when initial-contents `(:initial-element ,initial-contents))))))
(deft/method t/store-allocator (sym index-store-matrix) (size &rest initargs)
  (letv* ((() initargs))
    `(the index-store-matrix (make-array ,size :element-type 'index-type))))

(deft/method (t/store-allocator #'linear-storep) (sym tensor) (size &rest initargs)
  (letv* (((&key initial-element) initargs))
    (with-gensyms (sitm size-sym arr idx init)
      (let ((type (second (store-type sym))))
	`(let*-typed ((,size-sym (t/compute-store-size ,sym (let ((,sitm ,size))
							      (etypecase ,sitm
								(index-type ,sitm)
								(index-store-vector (lvec-foldr #'* (the index-store-vector ,sitm)))
								(cons (reduce #'* ,sitm))))))
		      ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
		      (,arr (make-array ,size-sym :element-type ',type :initial-element ,(if (subtypep type 'number) `(t/fid+ ,type) nil)) :type ,(store-type sym)))
	   ,@(when initial-element
		   `((very-quickly (loop :for ,idx :from 0 :below ,size-sym :do (t/store-set ,sym ,init ,arr ,idx)))))
	   ,arr)))))

(deft/method (t/store-allocator #'hash-table-storep) (sym stride-accessor) (size &rest initargs)
  (letv* (((&key size) initargs))
    `(make-hash-table :size ,size)))
;;
(deft/generic (t/store-ref #'subtypep) sym (store &rest idx))
(deft/generic (t/store-set #'subtypep) sym (value store &rest idx))

(define-setf-expander t/store-ref (sym store &rest idx &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion store env)
    (declare (ignore newval setter))
    (with-gensyms (nval)
      (values dummies vals `(,nval)
	      `(t/store-set ,sym ,nval ,getter ,@idx)
	      `(t/store-ref ,sym ,getter ,@idx)))))

(deft/method (t/store-ref #'linear-storep) (sym tensor) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (clinear-storep sym)
	(using-gensyms (decl (store idx) (2idx))
	  `(let (,@decl)
	     (declare (type ,(store-type sym) ,store)
		      (type index-type ,idx))
	     (let-typed ((,2idx (* 2 ,idx) :type index-type))
	       (values (complex (aref ,store ,2idx) (aref ,store (1+ ,2idx))) t))))
	`(values (aref (the ,(store-type sym) ,store) (the index-type ,idx)) t))))

(deft/method (t/store-set #'linear-storep) (sym tensor) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((idx (car idx)))
    (if (clinear-storep sym)
	(using-gensyms (decl (store idx value) (2idx))
	  `(let (,@decl)
	     (declare (type ,(store-type sym) ,store)
		      (type ,(field-type sym) ,value)
		      (type index-type ,idx))
	     (let-typed ((,2idx (* 2 ,idx) :type index-type))
	       (setf (aref ,store ,2idx) (cl:realpart ,value)
		     (aref ,store (1+ ,2idx)) (cl:imagpart ,value)))
	     ,value))
	`(setf (aref (the ,(store-type sym) ,store) (the index-type ,idx)) ,value))))
;;
(deft/method (t/store-ref #'hash-table-storep) (sym stride-accessor) (store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  `(the (values ,(field-type sym) boolean) (gethash (the index-type ,(car idx)) (the hash-table ,store) (t/fid+ ,(field-type sym)))))

(deft/method (t/store-set #'hash-table-storep) (sym stride-accessor) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (let ((fty (field-type sym))
	(idx (car idx)))
    (using-gensyms (decl (store idx value))
      `(let (,@decl)
	 (declare (type ,fty ,value))
	 (if (t/f= ,fty ,value (t/fid+ ,fty))
	     (progn (remhash ,idx (the hash-table ,store)) (t/fid+ ,fty))
	     (setf (gethash (the index-type ,idx) (the hash-table ,store)) (the ,(field-type sym) ,value)))))))
;;
(deft/generic (with-field-element #'subtypep) sym (decl &rest body))
(defmacro with-field-elements (sym decls &rest body)
  (if (null decls) `(progn ,@body)
      `(with-field-element ,sym ,(first decls)
	 (with-field-elements ,sym ,(cdr decls) ,@body))))

(deft/method with-field-element (sym tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(let-typed ((,var (t/store-allocator ,sym ,count :initial-element ,init) :type ,(store-type sym)))
       (locally ,@body))))
;;
;;A helper macro which takes of care of the class checking and stuff.
(eval-every

(defparameter *template-generated-methods* (make-hash-table :test 'equal))
(definline lazy-coerce (x output-type-spec)
  (if (typep x output-type-spec) x
      (let ((ret (copy x output-type-spec)))
	(when (slot-exists-p x 'memos) (iter (for (k v) in-hashtable (memos x)) (setf (gethash k (memos ret)) v)))
	ret)))

(defun cclass-max (&rest lst)
  (iter (for ele in lst) (with max)
	(when (or (null max) (and (coerceable? max ele) (or (not (coerceable? ele max))
							    (and (float-tensorp ele) (float-tensorp max)
								 (> (float-digits (coerce 0 (or (real-subtype (field-type ele)) (field-type ele))))
								    (float-digits (coerce 0 (or (real-subtype (field-type max)) (field-type max)))))))))
	  (setf max ele))
	(finally (return max))))
;;(cclass-max (tensor '(complex double-float)) (tensor '(complex double-float)))

(defmacro define-tensor-method (name (&rest args) &body body)
  (let* ((keypos (or (position-if (lambda (x) (member x cl:lambda-list-keywords)) args) (length args)))
	 (dispatch-args (subseq args 0 keypos))
	 (dispatch-sym (mapcar (lambda (x) (if (consp x) (first x) x)) dispatch-args))
	 (dispatch-key (mapcar (lambda (x) (if (consp x) (second x) t)) dispatch-args))
	 (generate-args (remove-if-not #'(lambda (x) (and (consp x) (cddr x))) dispatch-args))
	 (generate-groups (iter (for ele in generate-args) (unioning (list (third ele))))))
    (with-gensyms (xx yy coerce-types generate-dispatch generate-group-types value existsp type-methods func)
      `(eval-every
	 (letv* ((,value ,existsp (gethash ',name *template-generated-methods*)))
	   (if ,existsp
	       (if-let (,type-methods (assoc ',dispatch-key (cdr ,value) :test #'equal))
		 (iter (for ,func in (cdr ,type-methods))
		       (remove-method (function ,name) ,func)
		       (finally (setf (cdr ,type-methods) nil)))
		 (setf (cdr ,value) (list* (list ',dispatch-key) (cdr ,value))))
	       (setf (gethash ',name *template-generated-methods*) (list ',name (list ',dispatch-key)))))
	 ;;
	 (defmethod ,name (,@(mapcar (lambda (x) (if (consp x) (subseq x 0 2) x)) args))
	   (let* ((,generate-dispatch (list ,@(mapcar (lambda (x) (letv* (((arg default-class group &optional outputp) x :type (t nil t &optional t)))
								    `(list ',arg (let ((,xx (class-name (class-of ,arg))))
										   (assert (tensor-leafp ,xx) nil 'tensor-abstract-class :tensor-class ,xx)
										   ,xx)
									   ',group ,outputp)))
						      generate-args)))
		  (,generate-group-types (iter (for ,xx in ,generate-dispatch)
					       (with ,generate-group-types := (zip (list ,@(mapcar (lambda (x) `(quote ,x)) generate-groups))))
					       (let ((,yy (assoc (third ,xx) ,generate-group-types)))
						 (setf (cdr ,yy) (if (cdr ,yy) (cclass-max (cdr ,yy) (second ,xx)) (second ,xx)))
						 (assert (or (not (fourth ,xx)) (eql (cdr ,yy) (second ,xx))) nil "output type clash: don't know how to generate code for the given arguments."))
					       (finally (return ,generate-group-types))))
		  (,coerce-types (list ,@(mapcar (lambda (x) (cond ((atom x) t)
								   ((cddr x) `(cdr (assoc ',(third x) ,generate-group-types)))
								   (t `(quote ,(second x)))))
						 dispatch-args))))
	     (unless (find-method (function ,name) nil ,coerce-types nil)
	       (let ((,xx (or (assoc ',dispatch-key (cdr (gethash ',name *template-generated-methods*)) :test #'equal) (error "Method table missing from *template-generated-methods*!"))))
		 ;;(format t "Compiling ~a method for dispatch ~s." ',name ,coerce-types)
		 (push (compile-and-eval
			`(defmethod ,',name (,@(append (zip ',dispatch-sym ,coerce-types) ',(nthcdr keypos args)))
			   ,@(macrolet ((cl (,xx) `(or (cdr (assoc (third (assoc ',,xx ,',generate-dispatch)) ,',generate-group-types)) (error "Can't find class of ~a" ',,xx))))
				       (list ,@body))))
		       (cdr ,xx))))
	     (apply #',name (append (mapcar (lambda (,xx ,yy) (lazy-coerce ,xx ,yy)) (list ,@dispatch-sym) ,coerce-types)
				    ,(if (find '&rest args)
					 `(list* ,@(mapcar #'(lambda (x) (if (consp x) (first x) x)) (remove-if (lambda (x) (member x cl:lambda-list-keywords)) (butlast (nthcdr keypos args)))) ,(car (last args)))
					 `(list ,@(mapcar #'(lambda (x) (if (consp x) (first x) x)) (remove-if (lambda (x) (member x cl:lambda-list-keywords)) (nthcdr keypos args))))))))))))))
;;

;; (defgeneric testg (x &optional ele))
;; (define-tensor-method testg ((x dense-tensor :a) &optional (ele 1))
;;   `(t/copy! (t ,(cl x)) (coerce ele ',(field-type (cl x))) x)
;;   'x)

;; (testg (zeros 10))

;; (defgeneric copy!-test (x y))

;; (define-tensor-method copy!-test ((x dense-tensor :a) (y dense-tensor :b t))
;;   `(t/copy! (,(cl x) ,(cl y)) x y))

;; (define-tensor-method axpy-test (alpha (x dense-tensor :a) (y dense-tensor :a t))
;;   `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
;;      (declare (type ,(field-type (cl x)) alpha))
;;      ,(recursive-append
;;        (when (blas-tensorp (cl x))
;; 	 `(if-let (strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y)))
;; 	    (t/blas-axpy! ,(cl x) alpha x (first strd) y (second strd))))
;;        `(t/axpy! ,(cl x) alpha x y))))


(defgeneric store-ref (tensor idx)
  (:documentation  "Generic serial read access to the store."))
(define-tensor-method store-ref ((tensor tensor :x) idx)
  `(t/store-ref ,(cl tensor) (t/store ,(cl tensor) tensor) idx))

(defgeneric (setf store-ref) (value tensor idx))
(define-tensor-method (setf store-ref) (value (tensor tensor :x) idx)
  `(t/store-set ,(cl tensor) value (t/store ,(cl tensor) tensor) idx))

;;
(defgeneric store-size (tensor)
  (:documentation "
  Syntax
  ======
  (store-size tensor)

  Purpose
  =======
  Returns the number of elements the store of the tensor can hold
  (which is not necessarily equal to its vector length)."))
(define-tensor-method store-size ((tensor tensor :x))
  `(t/store-size ,(cl tensor) (slot-value tensor 'store)))

(defmethod total-size ((x dense-tensor))
  (t/total-size dense-tensor x))

(define-tensor-method total-size ((obj tensor :x))
  `(t/total-size ,(cl obj) obj))

;;Blas
(deft/generic (t/blas-lb #'subtypep) sym (i))
(deft/method t/blas-lb (sym blas-mixin) (i)
  (if (clinear-storep sym)
      (ecase i
	(1 '*complex-l1-fcall-lb*)
	(2 '*complex-l2-fcall-lb*)
	(3 '*complex-l3-fcall-lb*))
      (ecase i
	(1 '*real-l1-fcall-lb*)
	(2 '*real-l2-fcall-lb*)
	(3 '*real-l3-fcall-lb*))))
