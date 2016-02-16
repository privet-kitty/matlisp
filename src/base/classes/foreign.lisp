(in-package #:matlisp)

(defclass foreign-tensor (dense-tensor)  ()
  (:documentation "Object which holds all values of its components, with a foreign-vector store.")
  (:metaclass tensor-class))

(with-memoization ()
  (memoizing
   (defun foreign-tensor (field)
     (or (if-let (class (find field (closer-mop:class-direct-subclasses (find-class 'foreign-tensor)) :key #'field-type))
	   (class-name class))
	 (let* ((super-classes (remove nil (list (if (member field '(single-float double-float (complex single-float) (complex double-float)) :test #'equal)
						     'blas-mixin)
						 'foreign-tensor)))
		(cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "MATLISP"))))
	   (compile-and-eval
	    `(progn
	       (defclass ,cl-name (,@super-classes) ()
		 (:metaclass tensor-class))
	       (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
	   cl-name)))))
;;
(deft/method t/store-allocator (cl foreign-tensor) (size &rest initargs)
  (error "cannot allocate store for ~a" cl))
(deft/method t/store-type (class foreign-tensor) (&optional size)
  (ffi:foreign-vector (ffi:lisp->ffc (field-type class))))
(deft/method t/store-size (cl foreign-tensor) (vec)
  (if (clinear-storep cl) `(/ (slot-value (the ,(store-type cl) ,vec) 'length) 2) `(slot-value (the ,(store-type cl) ,vec) 'length)))
;;
(deft/method t/store-ref (cl foreign-tensor) (store &rest idx)
   (assert (null (cdr idx)) nil "given more than one index for linear-store")
   (with-gensyms (store_ idx_)
     `(let-typed ((,idx_ (recursive-append ,(if (clinear-storep cl) `(cl:* 2 (the fixnum ,(car idx))) (car idx))))
		  (,store_ ,store))
	(declare (type index-type ,idx_)
		 (type ,(store-type cl) ,store_))
	,(if (clinear-storep cl)
	     `(complex ,(funcall (compiler-macro-function 'ffi:fvref) `(ffi:fvref (the ,(store-type cl) ,store_) (the fixnum ,idx_)) nil)
		       ,(funcall (compiler-macro-function 'ffi:fvref) `(ffi:fvref (the ,(store-type cl) ,store_) (the fixnum (cl:1+ ,idx_))) nil))
	     (funcall (compiler-macro-function 'ffi:fvref) `(ffi:fvref (the ,(store-type cl) ,store_) (the fixnum ,idx_)) nil)))))
(deft/method t/store-set (cl foreign-tensor) (value store &rest idx)
  (assert (null (cdr idx)) nil "given more than one index for linear-store")
  (with-gensyms (store_ idx_ value_)
    `(let-typed ((,idx_ (recursive-append ,(if (clinear-storep cl) `(cl:* 2 (the fixnum ,(car idx))) (car idx))))
		 (,store_ ,store)
		 (,value_ ,value))
       (declare (type index-type ,idx_)
		(type ,(store-type cl) ,store_)
		(type ,(field-type cl) ,value_))
       ,(if (clinear-storep cl)
	    `(progn
	       ,(funcall (compiler-macro-function '(setf ffi:fvref)) `((setf ffi:fvref) (the ,(real-subtypep (field-type cl)) (cl:realpart ,value_))
								       (the ,(store-type cl) ,store_) (the fixnum ,idx_)) nil)
	       ,(funcall (compiler-macro-function '(setf ffi:fvref)) `((setf ffi:fvref) (the ,(real-subtypep (field-type cl)) (cl:imagpart ,value_))
								       (the ,(store-type cl) ,store_) (the fixnum (cl:1+ ,idx_))) nil)
	       ,value_)
	    (funcall (compiler-macro-function '(setf ffi:fvref)) `((setf ffi:fvref) (the ,(field-type cl) ,value_)
								   (the ,(store-type cl) ,store_) (the fixnum ,idx_)) nil)))))
;;
(deft/method with-field-element (cl foreign-tensor) (decl &rest body)
  (destructuring-bind (var init &optional (count 1)) decl
    (with-gensyms (idx size point init_)
      (let ((type (ffi:ffc->cffi (ffi:ffi-type (store-type cl)))))
	`(let ((,size (t/compute-store-size ,cl ,count)))
	   (cffi:with-foreign-object (,point ,type ,size)
	     (let ((,var (make-instance ',(store-type cl) :ptr ,point :length ,size)))
	       ,@(when init
		   `((let-typed ((,init_ ,init :type ,(field-type cl)))
		       (loop :for ,idx :from 0 :below ,size
			  :do (t/store-set ,cl ,init_ ,var ,idx)))))
	       (locally
		   ,@body))))))))
;;
;; (progn
;;   (defclass foreign-tensor-doub (foreign-tensor) ()
;;     (:metaclass tensor-class))
;;   (setf (slot-value (find-class 'foreign-tensor-doub) 'field-type) 'double-float)

;;   (defclass foreign-tensor-cdoub (foreign-tensor) ()
;;     (:metaclass tensor-class))
;;   (setf (slot-value (find-class 'foreign-tensor-cdoub) 'field-type) '(complex double-float))
;;   )

;; (in-readtable :infix-dispatch-table)
;; (with-field-element #.(foreign-tensor 'double-float) (vec nil (expt 3 2))
;;   (let* ((dims (idxv 3 3))
;; 	 (strd (make-stride dims))
;; 	 (ten (t/zeros #.(foreign-tensor 'double-float) (list 4 3) vec)))
;;     (copy! (copy! (randn '(3 3)) ten) (zeros '(3 3)))))

;; (with-field-element foreign-tensor-doub (vec nil (expt 1000 2))
;;   (let* ((dims (idxv 1000 1000))
;; 	 (strd (make-stride dims))
;; 	 (ten (make-instance 'foreign-tensor-doub :head 0 :dimensions dims :strides strd :store vec)))
;;     (let ((*real-l1-fcall-lb* 0))
;;       (time (copy! (zeros '(1000 1000)) ten)))
;;     ten))
    
;;   (t/copy! (#. (tensor 'double-float) foreign-tensor-doub) (randn '(3 3)) (make-instance 'foreign-tensor-doub :head 0 :dimensions dims :strides strd :store vec )))
