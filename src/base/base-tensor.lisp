(in-package #:matlisp)

;;Alias for fixnum.
(deftype index-type () 'fixnum)
(deftype index-store (&optional (size '*)) `(simple-array index-type ,size))
(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))
(deftype index-store-matrix (&optional (m '*) (n '*)) `(simple-array index-type (,m ,n)))
;;
(defclass base-accessor ()
  ((dimensions :initarg :dimensions :type index-store-vector :documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")))

(declaim (ftype (function (base-accessor &optional (or boolean index-type)) (or index-store-vector index-type list)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-accessor x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'dimensions)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (order x) nil 0))))
    (t (lvec->list (the index-store-vector (slot-value x 'dimensions))))))
;;Can this be moved out into MOP ?
(defmethod initialize-instance :after ((x base-accessor) &rest initargs)
  (declare (ignore initargs))
  (when *check-after-initializing?*
    (very-quickly
      (loop :for i :from 0 :below (length (dimensions x))
	 :do (assert (> (dimensions x i) 0) nil 'tensor-invalid-dimension-value :argument i :dimension (dimensions x i) :tensor x)))))
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
  ((indices :initarg :indices :type index-store-matrix :documentation "Non-zero indices in the tensor.")
   (stride-hash :initarg :stride-hash :type index-store-vector :documentation "Strides in Column major order")
   (strides :initarg :strides :type index-store-vector :documentation "Strides in Column major order")
   (boundary :initform 0 :initarg :boundary :type index-type :documentation "Row bound for indices"))
  (:documentation "Bi-partite graph/Hypergraph/Factor/Co-ordinate store"))
(defclass graph-accessor (base-accessor)
  ((fence :initarg :fence :type index-store-vector :documentation "Start index for neighbourhood.")
   (neighbours :initarg :neighbours :type index-store-vector :documentation "Neighbour ids.")
   (transposep :initarg :transposep :initform nil :type boolean :documentation "Choose between row-column compressed forms."))
  (:documentation "Graph store via Adjacency lists; only works for matrices."))
;;Store types
(defclass simple-array-store-mixin () ())
(defclass hash-table-store-mixin () ())
;;
(defclass base-tensor () ())

(closer-mop:defclass tensor-class (standard-class)
  ((field-type :reader field-type)))
(closer-mop:defmethod closer-mop:validate-superclass ((class tensor-class) (superclass standard-class))  t)
(defmethod field-type ((class symbol)) (field-type (find-class class)))

(closer-mop:defclass tensor (base-tensor base-accessor)
  ((store :initarg :store :reader store :documentation "Storage for the tensor.")
   (memos :initform nil :documentation "Memoized attributes."))
  (:metaclass tensor-class)
  (:documentation "Object which directly holds the values of its components (or part thereof)."))
;;This is probably unnecessary now that the reader does not compile at read time.
(defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))

(declaim (ftype (function (tensor) hash-table) memos))
(definline memos (x)
  (declare (type tensor x))
  (or (slot-value x 'memos) ;;Create hash-table only when necessary
      (setf (slot-value x 'memos) (make-hash-table :test 'equal))))
;;
(defclass dense-tensor (tensor stride-accessor simple-array-store-mixin)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another."))
  (:metaclass tensor-class)
  (:documentation "Object which holds all values of its components, with a simple-vector store."))
(defclass blas-mixin () ()
  (:documentation "Mixin which indicates that there exist foreign-routines for an object of this type."))
(definline orphanize (x)
  (declare (type dense-tensor))
  (setf (slot-value x 'parent) nil) x)
;;
(defclass graph-tensor (tensor graph-accessor simple-array-store-mixin) ()
  (:metaclass tensor-class))
(defclass hash-tensor (tensor stride-accessor hash-table-store-mixin) ()
  (:metaclass tensor-class))
(defclass coordinate-tensor (tensor coordinate-accessor simple-array-store-mixin) ()
  (:metaclass tensor-class))
;;
(defclass vector-mixin () ())
(defclass matrix-mixin () ())
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
  (defmem tensor (field &optional tensor order)
    (let* ((tensor (or tensor 'dense-tensor)))
      (declare (type (member dense-tensor graph-tensor hash-tensor coordinate-tensor) tensor))
      (or (if-let (class (find field (closer-mop:class-direct-subclasses (find-class tensor)) :key #'(lambda (x) (field-type (class-name x)))))
	    (class-name class))
	  (let* ((super-classes (remove nil (list (if (and (eql tensor 'dense-tensor)
							   (member field '(single-float double-float (complex single-float) (complex double-float)) :test #'equal))
						      'blas-mixin)
						  tensor (case order (1 'vector-mixin) (2 'matrix-mixin)))))
		 (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "MATLISP"))))
	    (compile-and-eval
	     `(progn
		(defclass ,cl-name (,@super-classes) ()
		  (:metaclass tensor-class))
		(setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
	    cl-name)))))

(defun tensor-load-form (class)
  (let ((supclass (mapcar #'class-name (closer-mop:class-direct-superclasses (find-class class)))))
    (append
     (list (field-type class))
     (set-difference supclass '(tensor dense-tensor blas-mixin))
     (when (member 'stride-accessor supclass) (list (first (ensure-list (store-type class))))))))
;;
