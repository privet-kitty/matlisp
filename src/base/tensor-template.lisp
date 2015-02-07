(in-package #:matlisp)

;;
(deft/generic (t/store-type #'subtypep) sym (&optional size))
(eval-every
  (defun real-subtype (cl) (when (and (consp cl) (eql (first cl) 'complex)) (second cl)))
  (defun store-type (cl &optional (size '*)) (macroexpand-1 `(t/store-type ,cl ,size)))
  (defun linear-storep (cl) (eql (first (ensure-list (store-type cl))) 'simple-array))
  (defun hash-table-storep (x) (eql (store-type x) 'hash-table))
  (defun clinear-storep (x) (and (linear-storep x) (real-subtype (field-type x))))
  (defun tensor-leafp (x)
    (let* ((x (etypecase x (class x) (symbol (find-class x))))
	   (subclasses (closer-mop:class-direct-subclasses x)))
      (and
       (member (find-class 'tensor) (closer-mop:class-direct-superclasses x))
       (not (find-if #'(lambda (x) (intersection (mapcar #'find-class '(base-tensor tensor)) (closer-mop:class-direct-superclasses x))) subclasses))))))

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
(eval-every (defun complexified-type (type) (macroexpand-1 `(t/complexified-type ,type))))

(deft/method t/complexified-type (sym tensor) ()
  (letv* (((type accessor &optional store) (tensor-load-form sym)))
    (cond
      ((real-subtype type) sym)
      ((subtypep type 'real) (tensor (list 'complex type) accessor store))
      (t (error "Unknown complex type for ~a" sym)))))

;;Now we're just making up names
(deft/generic (t/realified-type #'subtypep) sym ())
(eval-every (defun realified-type (type) (macroexpand-1 `(t/realified-type ,type))))

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
(deft/generic (t/store-allocator #'subtypep) sym (size &optional initial-element))
;;
(deft/method (t/compute-store-size #'linear-storep) (sym tensor) (size)
  (if (clinear-storep sym) `(* 2 ,size) size))

(deft/method (t/store-size #'linear-storep) (sym tensor) (ele)
  (if (clinear-storep sym) `(/ (length ,ele) 2) `(length ,ele)))

(deft/method (t/store-size #'hash-table-storep) (sym stride-accessor) (ele)
  `(hash-table-size ,ele))

(deft/method t/store-allocator (sym index-store) (size &optional initial-element)
  `(the index-store (make-array ,size :element-type 'index-type ,@(when initial-element `(:initial-element ,initial-element)))))
(deft/method t/store-allocator (sym index-store-vector) (size &optional initial-element)
  `(the index-store-vector (make-array ,size :element-type 'index-type ,@(when initial-element `(:initial-element ,initial-element)))))
(deft/method t/store-allocator (sym index-store-matrix) (size &optional initial-element)
  `(the index-store-matrix (make-array ,size :element-type 'index-type ,@(when initial-element `(:initial-element ,initial-element)))))

(deft/method (t/store-allocator #'linear-storep) (sym tensor) (size &optional initial-element)
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
	 ,arr))))

(deft/method (t/store-allocator #'hash-table-storep) (sym stride-accessor) (size &optional initial-element)
  `(make-hash-table :size ,size))
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
	     (progn (remhash ,(car idx) (the hash-table ,store)) (t/fid+ ,fty))
	     (setf (gethash (the index-type ,(car idx)) (the hash-table ,store)) (the ,(field-type sym) ,value)))))))
;;
(deft/generic (with-field-element #'subtypep) sym (decl &rest body))
(defmacro with-field-elements (sym decls &rest body)
  (if (null decls) `(progn ,@body)
      `(with-field-element ,sym ,(first decls)
	 (with-field-elements ,sym ,(cdr decls) ,@body))))

(deft/method with-field-element (sym tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    `(let-typed ((,var (t/store-allocator ,sym ,count ,init) :type ,(store-type sym)))
       (locally ,@body))))
;;
;;A helper macro which takes of care of the class checking and stuff.
;; (defparameter *generated-methods* (make-hash-table))

;; (definline lazy-coerce (x out)
;;   (if (typep x out) x
;;       (copy x out)))

;; (defun cclass-max (lst)
;;   (let ((max nil))
;;     (loop :for ele :in lst
;;        :do (when (or (null max) (and (coerceable? max ele)
;; 				     (or (not (coerceable? ele max))
;; 					 (and (subtypep ele 'blas-numeric-tensor) (subtypep max 'blas-numeric-tensor)
;; 					      (> (float-digits (coerce 0 (store-element-type ele)))
;; 						 (float-digits (coerce 0 (store-element-type max))))))))
;; 	     (setf max ele)))
;;     max))

;; (defmacro define-tensor-method (name (&rest args) &body body)
;;   (let* ((inputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :input))) args)))
;; 	 (outputs (mapcar #'car (remove-if-not #'(lambda (x) (and (consp x) (eql (third x) :output))) args)))
;; 	 (iclsym (zipsym inputs))
;; 	 (oclsym (zipsym outputs))
;; 	 (dargs (let ((pos (position-if #'(lambda (x) (member x cl:lambda-list-keywords)) args)))
;; 		  (if pos (subseq args 0 pos) args))))
;;     (with-gensyms (x classes iclasses oclasses)
;;       `(progn
;; 	 (multiple-value-bind (val exists?) (gethash ',name *generated-methods*)
;; 	   (if exists?
;; 	       (let ((type-meths (assoc ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs) (cdr val) :test #'tree-equal)))
;; 		 (if type-meths
;; 		     (progn
;; 		       (loop :for ele in (cdr type-meths)
;; 			  :do (remove-method (symbol-function ',name) ele))
;; 		       (setf (cdr type-meths) nil))
;; 		     (setf (cdr val) (list* (list ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs)) (cdr val)))))
;; 	       (setf (gethash ',name *generated-methods*) (list ',name (list ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs))))))
;; 	 ;;
;; 	 (defmethod ,name (,@(mapcar #'(lambda (x) (if (consp x) (subseq x 0 2) x)) args))
;; 	   (let* (,@(mapcar #'(lambda (lst) `(,(car lst) (class-name (class-of ,(cadr lst))))) (append iclsym oclsym))
;; 		  (,iclasses (list ,@(mapcar #'car iclsym)))
;; 		    (,oclasses (list ,@(mapcar #'car oclsym)))
;; 		    (,classes (append ,iclasses ,oclasses)))
;; 	     (labels ((generate-code (class)
;; 			(let ((args (mapcar #'(lambda (x) (if (and (consp x) (member (third x) '(:input :output)))
;; 							      (list (car x) class)
;; 							      x))
;; 					    '(,@args)))
;; 			      (ebody (macrolet ((cl (,x)
;; 						  (let ((slook '(,@(mapcar #'(lambda (x) `(,(cadr x) class)) iclsym)
;; 								 ,@(mapcar #'(lambda (x) `(,(cadr x) class)) oclsym))))
;; 						    (or (cadr (assoc ,x slook)) (error "Can't find class of ~a" ,x)))))
;; 				       (list ,@body))))
;; 			  `(defmethod ,',name (,@args)
;; 			     ,@ebody))))
;; 	       (cond
;; 		 ((every #'(lambda (,x) (eql ,x (car ,classes))) ,classes)
;; 		  (assert (member (car ,classes) *tensor-type-leaves*)
;; 			  nil 'tensor-abstract-class :tensor-class ,classes)
;; 		  (let* ((method (compile-and-eval (generate-code (car ,classes))))
;; 			 (lst (assoc ',(mapcar #'(lambda (x) (if (consp x) (cadr x) t)) dargs) (cdr (gethash ',name *generated-methods*)) :test #'tree-equal)))
;; 		    (assert lst nil "Method table missing from *generated-methods* !")
;; 		    (setf (cdr lst) (list* method (cdr lst))))
;; 		  (,name ,@(mapcar  #'(lambda (x) (if (consp x) (car x) x)) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args))))
;; 		 ((and (every #'(lambda (,x) (eql ,x (car ,oclasses))) ,oclasses)
;; 		       (or (null ,oclasses) (coerceable? (cclass-max ,iclasses) (car ,oclasses))))
;; 		  (let* ((clm (or (car ,oclasses) (cclass-max ,iclasses)))
;; 			 ,@(mapcar #'(lambda (x) `(,x (lazy-coerce ,x clm))) inputs))
;; 		    (declare (ignorable clm))
;; 		    (,name ,@(mapcar  #'(lambda (x) (if (consp x) (car x) x)) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) args)))))
;; 		 (t
;; 		  (error "Don't know how to apply ~a to classes ~a, ~a." ',name ,iclasses ,oclasses))))))))))
;; ;;

;; ;; (defgeneric testg (a))
;; ;; (define-tensor-method testg ((x standard-tensor :output))
;; ;;   `(t/copy! (t ,(cl x)) 1 x)
;; ;;   'x)

;; ;; (defgeneric axpy-test (alpha x y))

;; ;; (define-tensor-method axpy-test (alpha (x standard-tensor :input) (y standard-tensor :output))
;; ;;   `(let ((alpha (t/coerce ,(field-type (cl x)) alpha)))
;; ;;      (declare (type ,(field-type (cl x)) alpha))
;; ;;      ,(recursive-append
;; ;;        (when (subtypep (cl x) 'blas-numeric-tensor)
;; ;;	 `(if-let (strd (and (call-fortran? x (t/l1-lb ,(cl x))) (blas-copyablep x y)))
;; ;;	    (t/blas-axpy! ,(cl x) alpha x (first strd) y (second strd))))
;; ;;        `(t/axpy! ,(cl x) alpha x y))))
;; ;;

;; (defgeneric store-ref (tensor idx)
;;   (:documentation  "Generic serial read access to the store.")
;;   (:method ((tensor tensor) idx)
;;     (let ((clname (class-name (class-of tensor))))
;;       (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
;;       (compile-and-eval
;;        `(defmethod store-ref ((tensor ,clname) idx)
;; 	  (t/store-ref ,clname (store tensor) idx))))
;;     (store-ref tensor idx)))

;; (defgeneric (setf store-ref) (value tensor idx)
;;   (:method (value (tensor tensor) idx)
;;     (let ((clname (class-name (class-of tensor))))
;;       (assert (member clname *tensor-type-leaves*) nil 'tensor-abstract-class :tensor-class clname)
;;       (compile-and-eval
;;        `(defmethod (setf store-ref) (value (tensor ,clname) idx)
;; 	  (t/store-set ,clname value (store tensor) idx)
;; 	  (t/store-ref ,clname (store tensor) idx))))
;;     (setf (store-ref tensor idx) value)))

;;
(defgeneric store-size (tensor)
  (:documentation "
  Syntax
  ======
  (store-size tensor)

  Purpose
  =======
  Returns the number of elements the store of the tensor can hold
  (which is not necessarily equal to its vector length).")
  (:method ((tensor tensor))
    (let ((clname (class-name (class-of tensor))))
      (assert (tensor-leafp (class-of tensor)) nil 'tensor-abstract-class)
      (compile-and-eval
       `(defmethod store-size ((tensor ,clname))
	  (t/store-size ,clname (slot-value tensor 'store))))
      (store-size tensor))))
;;
