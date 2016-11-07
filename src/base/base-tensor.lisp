(in-package #:matlisp)

;;Alias for fixnum.
(deftype index-type () 'fixnum)
(deftype index-store (&optional (size '*)) `(simple-array index-type ,size))
(deftype index-store-vector (&optional (size '*)) `(simple-array index-type (,size)))
(deftype index-store-matrix (&optional (m '*) (n '*)) `(simple-array index-type (,m ,n)))
;;
(eval-every
  (closer-mop:defclass base-accessor ()
    ((dimensions :initarg :dimensions :type index-store-vector :documentation "Dimensions of the vector spaces in which the tensor's arguments reside."))))

(declaim (ftype (function (base-accessor &optional (or boolean index-type)) (or index-store-vector index-type list)) dimensions))
(definline dimensions (x &optional idx)
  (declare (type base-accessor x))
  (typecase idx
    (null (the index-store-vector (slot-value x 'dimensions)))
    (index-type (the index-type (aref (the index-store-vector (slot-value x 'dimensions)) (modproj (or idx 0) (order x) nil 0))))
    (t (lvec->list (the index-store-vector (slot-value x 'dimensions))))))
;;Can this be moved out into MOP ?
(closer-mop:defmethod initialize-instance :after ((x base-accessor) &rest initargs)
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
(closer-mop:defclass stride-accessor (base-accessor)
  ((strides :initarg :strides :type index-store-vector :documentation "Strides for accesing elements of the tensor.")
   (head :initarg :head :initform 0 :type index-type :documentation "Head for the store's accessor."))
  (:documentation "Vanilla stride accessor."))
(closer-mop:defclass coordinate-accessor (base-accessor)
  ((indices :initarg :indices :type index-store-matrix :documentation "Non-zero indices in the tensor.")
   (strides :initarg :strides :type index-store-vector :documentation "Strides for accesing elements of the tensor.")
   (stride-hash :initarg :stride-hash :type index-store-vector :documentation "Strides in Column major order")
   (tail :initform 0 :initarg :boundary :type index-type :documentation "Row bound for indices"))
  (:documentation "Bi-partite graph/Hypergraph/Factor/Co-ordinate store"))
(closer-mop:defclass graph-accessor (base-accessor)
  ((fence :initarg :fence :type index-store-vector :documentation "Start index for neighbourhood.")
   (neighbors :initarg :neighbors :type index-store-vector :documentation "Neighbour ids.")
   (transposep :initarg :transposep :initform nil :type boolean :documentation "Choose between row-column compressed forms."))
  (:documentation "Graph store via Adjacency lists; only works for matrices."))
;;Store types
(closer-mop:defclass simple-vector-store-mixin () ())
(closer-mop:defclass hash-table-store-mixin () ())
;;
(closer-mop:defclass base-tensor () ())

(eval-every
  (closer-mop:defclass tensor-class (standard-class)
    ((field-type :reader field-type)))
  (closer-mop:defmethod closer-mop:validate-superclass ((class tensor-class) (superclass standard-class))  t)
  (closer-mop:defmethod field-type ((class symbol)) (field-type (find-class class))))

(closer-mop:defclass tensor (base-tensor base-accessor)
  ((store :initarg :store :reader store :documentation "Storage for the tensor.")
   (memos :initform nil :documentation "Memoized attributes."))
  (:metaclass tensor-class)
  (:documentation "Object which directly holds the values of its components (or part thereof)."))
;;This is probably unnecessary now that the reader does not compile at read time.
(closer-mop:defmethod make-load-form ((tensor base-tensor) &optional env)
  (make-load-form-saving-slots tensor :environment env))
(declaim (ftype (function (tensor) hash-table) memos))
(definline memos (x)
  (declare (type tensor x))
  (or (slot-value x 'memos) (setf (slot-value x 'memos) (make-hash-table :test 'equal))))
;;
(deftype sparse-tensor () `(or coordinate-tensor hash-tensor graph-tensor))
;;
(closer-mop:defclass stride-tensor (tensor stride-accessor) () (:metaclass tensor-class))
;;
(closer-mop:defclass dense-tensor (stride-tensor)
  ((parent :initform nil :initarg :parent :type (or null tensor) :documentation "This slot is bound if the tensor is the view of another."))
  (:metaclass tensor-class)
  (:documentation "Object which holds all values of its components."))
(closer-mop:defclass hash-tensor (stride-tensor hash-table-store-mixin)
  ((stride-pivot :initarg :stride-pivot :type index-store-vector :documentation "This slot is used to invert the hash."))
  (:metaclass tensor-class))
(definline orphanize (x)
  (declare (type dense-tensor))
  (setf (slot-value x 'parent) nil) x)
;;
(closer-mop:defclass simple-dense-tensor (dense-tensor simple-vector-store-mixin) ()
  (:metaclass tensor-class)
  (:documentation "Dense tensor with simple-vector store."))

(closer-mop:defclass external-mixin () ())
(closer-mop:defclass blas-mixin (external-mixin) ()
  (:documentation "Mixin which indicates that there exist foreign-routines for an object of this type."))
;;
(closer-mop:defclass graph-tensor (tensor graph-accessor) ()
  (:metaclass tensor-class))
(closer-mop:defclass simple-graph-tensor (graph-tensor simple-vector-store-mixin) ()
  (:metaclass tensor-class))
;;
(closer-mop:defclass coordinate-tensor (tensor coordinate-accessor) ()
  (:metaclass tensor-class))
(closer-mop:defclass simple-coordinate-tensor (coordinate-tensor simple-vector-store-mixin) ()
  (:metaclass tensor-class))
;;
(closer-mop:defclass vector-mixin () ())
(closer-mop:defclass matrix-mixin () ())
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
	       (loop :for val :in subscripts
		  :for i :of-type index-type := 0 :then (1+ i)
		  :do (unless (or (eq val '*) (eq val (aref dims i)))
			(return nil))
		  :finally (return (when (= (1+ i) rank) t))))
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
(deftype tensor-type (field &key tensor order square)
  (let ((types (remove nil
		       (list (tensor field (match tensor
					     ('* nil)
					     (_ tensor)))
			     (ematch order
			       (1 `(satisfies tensor-vectorp))
			       (2 `(satisfies tensor-matrixp))
			       ('* nil))
			     (ematch square
			       ((or '* nil) nil)
			       (t `(satisfies tensor-squarep)))))))
    (if (cdr types) (list* 'and types) (car types))))

(closer-mop:defgeneric tensor-generator (field tensor))

(with-memoization ()
  (memoizing
   (defun tensor (field &optional tensor &aux (tensor (or tensor 'simple-dense-tensor)))
     (or (if-let ((class (find field (closer-mop:class-direct-subclasses (find-class tensor))  :key #'(lambda (x) (if (slot-boundp x 'field-type) (field-type (class-name x)))))))
	   (class-name class))
	 (tensor-generator field tensor)))))

(closer-mop:defmethod tensor-generator (field (tensor (eql 'simple-dense-tensor)))
  (let* ((super-classes (remove nil (list (if (member field '(single-float double-float (complex single-float) (complex double-float)) :test #'equal) 'blas-mixin) tensor #+nil (case order (1 'vector-mixin) (2 'matrix-mixin)))))
	 (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "MATLISP"))))
    (compile-and-eval
     `(progn
	(closer-mop:defclass ,cl-name (,@super-classes) () (:metaclass tensor-class))
	(setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))

(closer-mop:defmethod tensor-generator (field (tensor symbol))
  (assert (member tensor '(simple-graph-tensor hash-tensor simple-coordinate-tensor)) nil 'invalid-arguments)
  (let* ((super-classes (list tensor #+nil (case order (1 'vector-mixin) (2 'matrix-mixin))))
	 (cl-name (intern (format nil "<~{~a~^ ~}: ~a>" super-classes field) (find-package "MATLISP"))))
    (compile-and-eval
     `(progn
	(closer-mop:defclass ,cl-name (,@super-classes) ()
	  (:metaclass tensor-class))
	(setf (slot-value (find-class ',cl-name) 'field-type) ',field)))
    cl-name))
;;This is useful for Eigenvalue decompositions
(closer-mop:defgeneric complexified-tensor (class)
  (:method ((class-name symbol))
    (complexified-tensor (find-class class-name))))

(with-memoization ()
  (memoizing
   (closer-mop:defmethod complexified-tensor ((class tensor-class))
     (cond
       ((subtypep (field-type class) 'cl:complex) class)
       ((subtypep (field-type class) 'cl:real)
	(let* ((field `(cl:complex ,(field-type class)))
	       (super-classes (closer-mop:class-direct-superclasses class))
	       (siblings (apply #'intersection (mapcar #'closer-mop:class-direct-subclasses super-classes))))
	  (or (find field siblings :test #'equal :key #'field-type)
	      (let ((cl-name (intern (format nil "<~{~a~^ ~}: ~a>" (mapcar #'class-name super-classes) field) (find-package "MATLISP"))))
		(compile-and-eval
		 `(prog1
		      (closer-mop:defclass ,cl-name (,@(mapcar #'class-name super-classes)) ()
			(:metaclass tensor-class))
		    (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))))))
       (t (error "Unknown complex tensor for ~a" class))))))
(closer-mop:defmethod complexified-tensor :around ((class tensor-class))
  (class-name (call-next-method)))
;;Now we're just making up names
(closer-mop:defgeneric realified-tensor (class)
  (:method ((class-name symbol))
    (realified-tensor (find-class class-name)))
  (:method :around ((class tensor-class))
    (class-name (call-next-method))))
(with-memoization ()
  (memoizing
   (closer-mop:defmethod realified-tensor ((class tensor-class))
     (cond
       ((subtypep (field-type class) 'cl:real) class)
       ((match (field-type class)
	  ((lambda-list 'cl:complex field)
	   (letv* ((super-classes (closer-mop:class-direct-superclasses class))
		   (siblings (reduce #'intersection (mapcar #'closer-mop:class-direct-subclasses super-classes))))
	     (or (find field siblings :test #'equal :key #'field-type)
		 (let ((cl-name (intern (format nil "<~{~a~^ ~}: ~a>" (mapcar #'class-name super-classes) field) (find-package "MATLISP"))))
		   (compile-and-eval
		    `(prog1
			 (closer-mop:defclass ,cl-name (,@(mapcar #'class-name super-classes)) ()
			   (:metaclass tensor-class))
		       (setf (slot-value (find-class ',cl-name) 'field-type) ',field)))))))))
       (t (error "Unknown real tensor for ~a" class))))))
;;
