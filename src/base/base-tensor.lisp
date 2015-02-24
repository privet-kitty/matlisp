(in-package #:matlisp)

;;Alias for fixnum.
(deftype index-type () 'fixnum)
(deftype index-store (&optional (size '*)) `(simple-array index-type ,size))
(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))
(deftype index-store-matrix (&optional (m '*) (n '*)) `(simple-array index-type (,m ,n)))
;;
(defclass base-tensor () ())
;;
(defparameter *accessor-types* '(stride-accessor coordinate-accessor graph-accessor))

(defclass base-accessor ()
  ((dimensions :initarg :dimensions :type index-store-vector :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")))

(declaim (ftype (function (base-accessor &optional (or boolean index-type)) (or index-store-vector index-type list)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-accessor x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'dimensions)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (order x) nil 0))))
    (t (lvec->list (the index-store-vector (slot-value x 'dimensions))))))

(defmethod initialize-instance :after ((x base-accessor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (very-quickly
      (loop :for i :from 0 :below (length (dimensions x))
	 :do (assert (> (dimensions x i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (dimensions x i) :tensor x)))))

(defgeneric total-size (obj)
  (:method ((x base-accessor))
    (lvec-foldr #'(lambda (x y) (declare (type index-type x y)) (the index-type (* x y))) (the index-store-vector (dimensions x))))
  (:method ((obj sequence)) (length obj))
  (:method ((arr array)) (array-total-size arr)))
;;We use order (opposed to CL convention) so as not to cause confusion with matrix rank.
(definline order (x)
  (declare (type base-accessor x))
  (length (the index-store-vector (slot-value x 'dimensions))))
;;
(defclass stride-accessor (base-accessor)
  ((strides :initarg :strides :type index-store-vector :documentation "Strides for accesing elements of the tensor.")
   (head :initarg :head :initform 0 :type index-type :documentation "Head for the store's accessor."))
  (:documentation "Vanilla stride accessor."))

(defclass coordinate-accessor (base-accessor)
  ((indices :initarg :indices :type index-store-matrix :documentation "Non-zero indices in the tensor."))
  (:documentation "Bi-partite graph/Hypergraph/Factor/Co-ordinate store"))

(defclass graph-accessor (base-accessor)
  ((fence :initarg :fence :type index-store-vector :documentation "Start index for neighbourhood.")
   (neighbours :initarg :neighbours :type index-store-vector :documentation "Neighbour ids.")
   (transposep :initarg :transposep :initform nil :type boolean :documentation "Choose between row-column compressed forms."))
  (:documentation "Graph store via Adjacency lists; only works for matrices."))
;;
(defclass tensor (base-tensor base-accessor)
  ((store :initarg :store :reader store :documentation "Storage for the tensor.")
   (memos :initform nil :documentation "Memoized attributes."))
  (:documentation "Basic tensor class."))

(defclass dense-tensor (tensor)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another.")))

(definline orphanize (x)
  (declare (type dense-tensor))
  (setf (slot-value x 'parent) nil)
  x)
;;I have no idea what this does, or why we want it (inherited from standard-matrix.lisp)
(defmethod make-load-form ((tensor tensor) &optional env)
  "
  MAKE-LOAD-FORM allows us to determine a load time value for
  tensor, for example #.(make-tensors ...)"
  (make-load-form-saving-slots tensor :environment env))

(declaim (ftype (function (tensor) hash-table) memos))
(definline memos (x)
  (declare (type tensor x))
  ;;Create hash-table only when necessary
  (or (slot-value x 'memos)
      (setf (slot-value x 'memos) (make-hash-table :test 'equal))))
;;
(defun tensor-typep (tensor subs)
  "
  Syntax
  ======
  (tensor-typep tensor subscripts)

  Purpose
  =======
  Check if the given tensor is of a particular size in particular
  arguments.

  Examples
  ========
  Checking for a vector:
  > (tensor-typep ten '(class-name *))

  Checking for a matrix with 2 columns:
  > (tensor-typep ten '(real-tensor (* 2)))

  "
  (declare (type base-accessor tensor))
  (destructuring-bind (cls &optional subscripts) (ensure-list subs)
    (and (typep tensor cls)
	 (if subscripts
	     (let-typed ((rank (order tensor) :type index-type)
			 (dims (dimensions tensor) :type index-store-vector))
	       (very-quickly
		 (loop :for val :in subscripts
		    :for i :of-type index-type := 0 :then (1+ i)
		    :do (unless (or (eq val '*) (eq val (aref dims i)))
			  (return nil))
		    :finally (return (when (= (1+ i) rank) t)))))
	     t))))

(definline tensor-matrixp (ten)
  (declare (type base-accessor ten))
  (= (order ten) 2))

(definline tensor-vectorp (ten)
  (declare (type base-accessor ten))
  (= (order ten) 1))

(definline tensor-squarep (tensor)
  (declare (type base-accessor tensor))
  (let-typed ((dims (dimensions tensor) :type index-store-vector))
    (loop :for i :from 1 :below (length dims)
       :do (unless (= (aref dims i) (aref dims 0)) (return nil))
       :finally (return t))))

(deftype tensor-vector () `(and tensor (satisfies tensor-vectorp)))
(deftype tensor-matrix () `(and tensor (satisfies tensor-matrixp)))
(deftype tensor-square-matrix () `(and tensor (satisfies tensor-matrixp) (satisfies tensor-squarep)))
;;
(with-memoization ()
  (defmem tensor (field &optional accessor store)
    (let* ((accessor (or accessor 'stride-accessor))
	   (store (or store (and (eql accessor 'stride-accessor) 'simple-array))))
      (assert (and (member accessor *accessor-types*) (or (not store) (and (eql accessor 'stride-accessor) (member store '(simple-array hash-table))))) nil 'invalid-arguments)
      (or
       (find-if #'(lambda (x)
		    (and (eql (field-type x) field)
			 (or (not (eql accessor 'stride-accessor)) (eql (first (ensure-list (store-type x))) store))
			 (and (= (length (closer-mop:class-direct-superclasses (find-class x))) 2))))
		(mapcar #'class-name (apply #'intersection (mapcar #'closer-mop:class-direct-subclasses (mapcar #'find-class `(tensor ,accessor))))))
       (let ((cl (intern (format nil "~{~a~^ ~}" (remove nil (list field accessor store))))))
	 (compile-and-eval `(defclass ,cl (,(if (equal (list accessor store) '(stride-accessor simple-array)) 'dense-tensor 'tensor) ,accessor) ()))
	 (compile-and-eval `(deft/method t/field-type (sym ,cl) () ',field))
	 (case store
	   (simple-array (compile-and-eval `(deft/method t/store-type (sym ,cl) (&optional (size '*))
					      `(simple-array ,(or (real-subtype (field-type sym)) (field-type sym)) (,size)))))
	   (hash-table (compile-and-eval `(deft/method t/store-type (sym ,cl) (&optional (size '*))
					    'hash-table))))
	 cl)))))

(defun tensor-load-form (class)
  (let ((supclass (mapcar #'class-name (closer-mop:class-direct-superclasses (find-class class)))))
    (append
     (list (field-type class))
     (set-difference supclass '(tensor dense-tensor))
     (when (member 'stride-accessor supclass) (list (first (ensure-list (store-type class))))))))
