(in-package #:matlisp)

;;A helper macro which takes of care of the class checking and stuff.
(eval-every

(definline lazy-coerce (x output-type-spec)
  (if (typep x output-type-spec) x
      (let ((ret (copy x output-type-spec)))
	(when (slot-exists-p x 'memos) (iter (for (k v) in-hashtable (memos x)) (setf (gethash k (memos ret)) v)))
	ret)))

(defun real-subtypep (type) (match type ((λlist 'cl:complex &optional (type t)) type)))
(defun cclass-max (&rest lst)
  (iter (for ele in lst) (with max)
	(when (or (null max)
		  (and (coerceable? max ele)
		       (or (not (coerceable? ele max))
			   (and (float-tensorp ele) (float-tensorp max)
				(> (float-digits (coerce 0 (or (real-subtypep (field-type ele)) (field-type ele))))
				   (float-digits (coerce 0 (or (real-subtypep (field-type max)) (field-type max)))))))))
	  (setf max ele))
	(finally (return max))))
;;(cclass-max (tensor '(complex double-float)) (tensor '(complex double-float)))

;;MOP layer to fix issues with generation and subclassing.
(closer-mop:defclass tensor-method-generator (closer-mop:standard-generic-function) ()
  (:metaclass closer-mop:funcallable-standard-class))
;;Specializers
(closer-mop:defclass classp-specializer (closer-mop:specializer)
  ((object-class :initform nil :initarg :object-class)
   (direct-methods :initform nil :reader closer-mop:specializer-direct-methods))
  (:documentation "Exact class specializer."))
(defmethod print-object ((obj classp-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a" (class-name (slot-value obj 'object-class)))))

(defmethod closer-mop:add-direct-method ((specializer classp-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod closer-mop:remove-direct-method ((specializer classp-specializer) method)
  (setf (slot-value specializer 'direct-methods)
	(remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj classp-specializer) &optional env)
  #+nil(make-load-form-saving-slots obj :environment env)
  (values `(classp-specializer ',(class-name (slot-value obj 'object-class))) nil))
;;
(closer-mop:defclass group-specializer (closer-mop:specializer)
  ((object-class :initform nil :initarg :object-class)
   (group-name :initform nil :initarg :group-name)
   (direct-methods :initform nil :reader closer-mop:specializer-direct-methods))
  (:documentation "Applicable only if for each group-specializer with distinct @argument{group-name}, the classes of the respective argument are the same."))
(defmethod print-object ((obj group-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a, ~a" (class-name (slot-value obj 'object-class)) (slot-value obj 'group-name))))

(defmethod closer-mop:add-direct-method ((specializer group-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod closer-mop:remove-direct-method ((specializer group-specializer) method)
  (setf (slot-value specializer 'direct-methods)
	(remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj group-specializer) &optional env)
  #+nil(make-load-form-saving-slots obj :environment env)
  (values `(group-specializer ',(class-name (slot-value obj 'object-class)) ',(slot-value obj 'group-name)) nil))
;;
(defparameter *specializer-table* (make-hash-table :test 'equal))
(with-memoization (*specializer-table*)
  (memoizing
   (defun classp-specializer (class-name)
     (make-instance 'classp-specializer :object-class (find-class class-name))))
  (memoizing
   (defun group-specializer (class-name group-name)
     (make-instance 'group-specializer :object-class (find-class class-name) :group-name (the keyword group-name)))))

;;
(defmethod closer-mop:compute-applicable-methods-using-classes ((gf tensor-method-generator) required-classes)
  (iter mc
	(for m in (closer-mop:generic-function-methods gf))
	(with class-info-enoughp = t)
	(iter (for s in (closer-mop:method-specializers m))
	      (for c in required-classes) (with group-keys = nil)
	      (always
	       (etypecase s
		 (class (subtypep c s))
		 (closer-mop:eql-specializer (and (eql c (class-of (closer-mop:eql-specializer-object s)))
						  (not (setf class-info-enoughp nil))))
		 (group-specializer
		  (let ((key-name (slot-value s 'group-name)))
		    (if-let (key (assoc key-name group-keys))
		      (eql (cdr key) c)
		      (when (subtypep c (slot-value s 'object-class))
			(push (cons key-name c) group-keys) t))))
		 (classp-specializer (eq c (slot-value s 'object-class)))))
	      (finally (in mc (collect m into applicable-methods))))
	(finally (return-from mc
		   (values (sort (copy-list applicable-methods) #'(lambda (m1 m2) (method-more-specific-p m1 m2 required-classes)))
			   class-info-enoughp)))))

#+nil
(defmethod compute-applicable-methods ((gf tensor-method-generator) arguments)
  (iter mc
	(for m in (closer-mop:generic-function-methods gf))
	(iter (for s in (closer-mop:method-specializers m))
	      (for c in required-classes) (with group-keys = nil)
	      (always
	       (etypecase s
		 (class (subtypep c s))
		 (closer-mop:eql-specializer (and (eql c (class-of (closer-mop:eql-specializer-object s)))
						  (not (setf class-info-enoughp nil))))
		 (group-specializer
		  (let ((key-name (slot-value s 'group-name)))
		    (if-let (key (assoc key-name group-keys))
		      (eql (cdr key) c)
		      (when (subtypep c (slot-value s 'object-class))
			(push (cons key-name c) group-keys) t))))
		 (classp-specializer (eq c (slot-value s 'object-class)))))
	      (finally (in mc (collect m into applicable-methods))))
	(finally (return-from mc
		   (values (sort (copy-list applicable-methods) #'(lambda (m1 m2) (method-more-specific-p m1 m2 required-classes)))
			   class-info-enoughp)))))
;;Add closer-mop:compute-applicable-methods
;;Borrowed from AMOP p.122
(defun method-more-specific-p (method1 method2 required-classes)
  (map nil #'(lambda (spec1 spec2 arg-class)
	       (unless (or (eq spec1 spec2)
			   (cart-typecase (spec1 spec2)
			     ((classp-specializer classp-specializer) (eq (slot-value spec1 'object-class) (slot-value spec1 'object-class)))
			     ((group-specializer group-specializer) (and (eq (slot-value spec1 'object-class) (slot-value spec1 'object-class))
									 (eq (slot-value spec1 'group-name) (slot-value spec1 'group-name))))))
		 (return-from method-more-specific-p (sub-specializer-p spec1 spec2 arg-class))))
       (closer-mop:method-specializers method1)
       (closer-mop:method-specializers method2)
       required-classes))

(defun sub-specializer-p (spec1 spec2 arg-class)
  (cart-typecase (spec1 spec2)
    ((class class) (not (null (find spec2 (cdr (member spec1 (closer-mop:class-precedence-list arg-class)))))))
    ((classp-specializer classp-specializer) (sub-specializer-p (the class (slot-value spec1 'object-class)) (the class (slot-value spec2 'object-class)) arg-class))
    ;;classp-specializer in list if spec1.object-class = arg-class
    ((classp-specializer class) t)
    ((group-specializer class)
     (if (or (eq (slot-value spec1 'object-class) spec2)
	     (sub-specializer-p (slot-value spec1 'object-class) spec2 arg-class))
	 t nil))
    ((class group-specializer) (sub-specializer-p spec1 (slot-value spec2 'object-class) arg-class))
    ((classp-specializer group-specializer)
     (or (eq (slot-value spec1 'object-class) (slot-value spec2 'object-class))
	 (sub-specializer-p (slot-value spec1 'object-class) (slot-value spec2 'object-class) arg-class)))
    ((group-specializer classp-specializer)
     (sub-specializer-p (slot-value spec1 'object-class) (slot-value spec2 'object-class) arg-class))
    ((closer-mop:eql-specializer t) t)))

(defparameter *template-generated-methods* (make-hash-table :test 'equal))
;;(closer-mop:subclassp (find-class (tensor 'double-float)) (find-class 'base-tensor))
(defmacro define-tensor-method (name (&rest args) &body body)
  (let* ((keypos (or (position-if (lambda (x) (member x cl:lambda-list-keywords)) args) (length args)))
	 (dispatch-args (subseq args 0 keypos))
	 (dispatch-sym (mapcar (lambda (x) (if (consp x) (first x) x)) dispatch-args))
	 (dispatch-key (mapcar (lambda (x) (if (consp x) (second x) t)) dispatch-args))
	 (generate-args (remove-if-not #'(lambda (x) (and (consp x) (cddr x))) dispatch-args))
	 (generate-groups (iter (for ele in generate-args) (unioning (list (third ele))))))
    (with-gensyms (xx value existsp type-methods func)
      `(eval-every
	 ;;clear methods
	 (letv* ((,value ,existsp (gethash ',name *template-generated-methods*)))
	   (if ,existsp
	       (if-let (,type-methods (assoc ',dispatch-key (cdr ,value) :test #'equal))
		 (iter (for ,func in (cdr ,type-methods))
		       (remove-method (function ,name) ,func)
		       (finally (setf (cdr ,type-methods) nil)))
		 (setf (cdr ,value) (list* (list ',dispatch-key) (cdr ,value))))
	       (setf (gethash ',name *template-generated-methods*) (list ',name (list ',dispatch-key)))))
	 ;;generic type coercer.
	 ,@(let* ((coerce-groups (remove-if-not #'(lambda (x) (< 1 (length (remove-if-not #'(lambda (y) (eql x (third y))) generate-args)))) generate-groups))
		  (sym (zipsym coerce-groups)))
	     (when coerce-groups
	       `((defmethod ,name (,@(mapcar #'(lambda (x) (if (consp x) (subseq x 0 2) x)) (subseq args 0 keypos)) ,@(subseq args keypos))
		   (let (,@(iter (for (ts g) in sym)
				 (collect `(,ts ,(rec c+ ((lst (remove-if-not #'(lambda (x) (eql g (third x))) generate-args)) &aux (tmp (gensym)))
						      (when lst `(cclass-max (type-of ,(first (car lst))) ,(c+ (cdr lst)))))))))
		     ,@(iter (for (ts g) in sym)
			     (appending (mapcar #'(lambda (x)
						    `(assert (eql ,ts (type-of ,(first x))) nil "output type clash: don't know how to generate code for the given arguments."))
						(remove-if-not #'(lambda (x) (and (eql g (third x)) (fourth x))) generate-args))))
		     ,@(let ((dargs (mapcar #'(lambda (x)
						(match x
						  ((λlist name dispatch (guard group (member group coerce-groups)) &optional destructive)
						   (if destructive name `(lazy-coerce ,name ,(first (rassoc (list group) sym :test #'equal)))))
						  ((list name dispatch) name)
						  (_ x)))
					    (subseq args 0 keypos))))
			    (if-let (rest-pos (position '&rest args))
			      `((apply #',name (list* ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos rest-pos) cl:lambda-list-keywords))
						      ,(elt args (1+ rest-pos)))))
			      `((,@(if (symbolp name) `(,name) `(funcall #',name)) ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos) cl:lambda-list-keywords)))))))))))
	 ;;method generator.
	 ,@(let ((sym (zipsym generate-groups)))
	     `((defmethod ,name (,@(mapcar (lambda (x) (match x
							 ((λlist name dispatch group &optional destructive)
							  `(,name ,(group-specializer dispatch group)))
							 (_ x)))
					   (subseq args 0 keypos))
				 ,@(subseq args keypos))
		 (let (,@(iter (for (tg g) in sym) (collect `(,tg (type-of ,(first (find-if #'(lambda (x) (eql (third x) g)) generate-args))))))
		       (,xx (or (assoc ',dispatch-key (cdr (gethash ',name *template-generated-methods*)) :test #'equal)
				(error "Method table missing from *template-generated-methods*!"))))
		   ;;(format t "Compiling ~a method for dispatch ~s." ',name ,coerce-types)
		   (push
		    (macrolet ((cl (,xx) (ecase ,xx ,@(mapcar #'(lambda (x) `(,(second x) (quote ,(first x))))  sym))))
		      (compile-and-eval
		       `(defmethod ,',name (,@(list ,@(mapcar (lambda (x) (match x
									    ((λlist name dispatch group &optional destructive)
									     `(list ',name (classp-specializer (cl ,group))))
									    (_ `(quote ,x))))
							      (subseq args 0 keypos)))
													  ,@',(subseq args keypos))
			  ,@(list ,@body))))
		    (cdr ,xx)))
		 ,@(let ((dargs (mapcar #'(lambda (x) (first (ensure-list x))) (subseq args 0 keypos))))
			(if-let (rest-pos (position '&rest args))
			  `((apply #',name (list* ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference (subseq args keypos rest-pos) cl:lambda-list-keywords))
						  ,(elt args (1+ rest-pos)))))
			  `((,@(if (symbolp name) `(,name) `(funcall #',name)) ,@dargs ,@(mapcar #'(lambda (x) (first (ensure-list x))) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) (subseq args keypos))))))))))))))
)
