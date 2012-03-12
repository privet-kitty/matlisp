;; Definitions of STANDARD-MATRIX
(in-package :matlisp)

;;
(declaim (inline allocate-integer4-store))
(defun allocate-integer4-store (size &optional (initial-element 0))
  "(ALLOCATE-INTEGER-STORE SIZE [INITIAL-ELEMENT]).  Allocates
integer storage.  Default INITIAL-ELEMENT = 0."
  (make-array size
	      :element-type 'integer4-matrix-element-type
	      :initial-element initial-element))

;;
(declaim (inline store-indexing))
(defun store-indexing (row col head row-stride col-stride)
  (declare (type (and fixnum (integer 0)) row col head row-stride col-stride))
  (the fixnum (+ head (the fixnum (* row row-stride)) (the fixnum (* col col-stride)))))

;;
(defclass standard-matrix ()
  ((number-of-rows
    :initarg :nrows
    :initform 0
    :accessor nrows
    :type fixnum
    :documentation "Number of rows in the matrix")
   (number-of-cols
    :initarg :ncols
    :initform 0
    :accessor ncols
    :type fixnum
    :documentation "Number of columns in the matrix")
   (number-of-elements
    :initform 0
    :accessor number-of-elements
    :type fixnum
    :documentation "Total number of elements in the matrix (nrows * ncols)")
   ;;
   (head
    :initarg :head
    :initform 0
    :accessor head
    :type fixnum
    :documentation "Head for the store's accessor.")
   (row-stride
    :initarg :row-stride
    :accessor row-stride
    :type fixnum
    :documentation "Row stride for the store's accessor.")
   (col-stride
    :initarg :col-stride
    :accessor col-stride
    :type fixnum
    :documentation "Column stride for the store's accessor.")
   (store-size
    :accessor store-size
    :type fixnum
    :documentation "Total number of elements needed to store the matrix.  (Usually
the same as nels, but not necessarily so!")
   (store
    :initarg :store
    :accessor store
    :documentation "The actual storage for the matrix.  It is typically a one dimensional
array but not necessarily so.  The float and complex matrices do use
1-D arrays.  The complex matrix actually stores the real and imaginary
parts in successive elements of the matrix because Fortran stores them
that way."))
  (:documentation "Basic matrix class."))

;;
(defmethod initialize-instance :after ((matrix standard-matrix) &rest initargs)
  (declare (ignore initargs))
  (let* ((n (nrows matrix))
	 (m (ncols matrix))
	 (h (head matrix))
	 (rs (row-stride matrix))
	 (cs (col-stride matrix))
	 (ss (store-size matrix))
	 (nxm (* n m)))
    (declare (type fixnum n m h rs cs nxm))
    ;;Error checking is good if we use foreign-pointers as store types.
    (cond
      ((<= n 0) (error "Number of rows must be > 0. Initialized with ~A." n))
      ((<= m 0) (error "Number of columns must be > 0. Initialized with ~A." m))
      ;;
      ((< h 0) (error "Head of the store must be >= 0. Initialized with ~A." h))
      ((< rs 0) (error "Row-stride of the store must be > 0. Initialized with ~A." rs))
      ((< cs 0) (error "Column-stride of the store must be > 0. Initialized with ~A." cs))
      ((<= ss 0) (error "Store-size must be > 0. Initialized with ~A." ss)))
    ;;Row-ordered by default.
    (when (or (= rs 0) (= cs 0))
      (setf (row-stride matrix) m)
      (setf (col-stride matrix) 1))
    
    (setf (number-of-elements matrix) nxm)))

;;
(defmacro matrix-ref (matrix row &optional col)
  (if col
      `(matrix-ref-2d ,matrix ,row ,col)
      `(matrix-ref-1d ,matrix ,row)))

;;
(defgeneric matrix-ref-1d (matrix store-idx)
  (:documentation "
  Syntax
  ======
  (matrix-REF-1d store store-idx)

  Purpose
  =======
  Return the element store-idx of the matrix store."))

#+nil(defmethod matrix-ref-1d :before ((matrix standard-matrix) (idx fixnum))
  (unless (< -1 (- idx (head matrix)) (number-of-elements matrix))
    (error "Requested index ~A is out of bounds.
Matrix only has ~A elements." idx (number-of-elements matrix))))

;;
(defgeneric (setf matrix-ref-1d) (value matrix idx))

#+nil(defmethod (setf matrix-ref-1d) :before ((value t) (matrix standard-matrix) (idx fixnum))
       (unless (< -1 idx (number-of-elements matrix))
	 (error "Requested index ~A is out of bounds.
Matrix only has ~A elements." idx (number-of-elements matrix))))

;;
(defgeneric matrix-ref-2d (matrix rows cols)
  (:documentation "
  Syntax
  ======
  (MATRIX-REF-2d store i j)

  Purpose
  =======
  Return the element
  (+
    (* (row-stride store) i)
    (* (col-stride store) j))
  of the store "))

(defmethod matrix-ref-2d :before ((matrix standard-matrix) (rows fixnum) (cols fixnum))
  (unless (and (< -1 rows (nrows matrix))
	       (< -1 cols (ncols matrix)))
    (error "Requested index (~A ~A) is out of bounds." rows cols)))

(defmethod matrix-ref-2d ((matrix standard-matrix) (rows fixnum) (cols fixnum))
  (matrix-ref-1d matrix (store-indexing rows cols (head matrix) (row-stride matrix) (col-stride matrix))))

;;
(defgeneric (setf matrix-ref-2d) (value matrix rows cols))

(defmethod (setf matrix-ref-2d) ((value t) (matrix standard-matrix) (rows fixnum) (cols fixnum))
  (setf (matrix-ref-1d matrix (store-indexing rows cols (head matrix) (row-stride matrix) (col-stride matrix))) value))

;;
(defgeneric row-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (ROW-VECTOR-P x)

  Purpose
  =======
  Return T if X is a row vector (number of columns is 1)"))

(declaim (inline row-vector-p))
(defmethod row-vector-p ((matrix standard-matrix))
  (= (nrows matrix) 1))

;;
(defgeneric col-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (COL-VECTOR-P x)
 
  Purpose
  =======
  Return T if X is a column vector (number of rows is 1)"))

(declaim (inline col-vector-p))
(defmethod col-vector-p ((matrix standard-matrix))
  (= (ncols matrix) 1))

;;
(defgeneric row-or-col-vector-p (matrix)
  (:documentation "
  Syntax
  ======
  (ROW-OR-COL-VECTOR-P x)

  Purpose
  =======
  Return T if X is either a row or a column vector"))

(declaim (inline row-or-col-vector-p))
(defmethod row-or-col-vector-p ((matrix standard-matrix))
  (or (row-vector-p matrix) (col-vector-p matrix)))

;;
(defgeneric square-matrix-p (matrix)
  (:documentation "
  Syntax
  ======
  (SQUARE-MATRIX-P x)

  Purpose
  =======
  Return T if X is square matrix"))

(declaim (inline square-matrix-p))
(defmethod square-matrix-p ((matrix standard-matrix))
  (= (nrows matrix) (ncols matrix)))

;;
(defgeneric size (matrix)
  (:documentation "
  Syntax
  ======
  (SIZE x)

  Purpose
  =======
  Return the number of rows and columns of the matrix X as a list"))

(defmethod size ((matrix standard-matrix))
  (list (nrows matrix) (ncols matrix)))

;;
(defgeneric transpose! (matrix)
  (:documentation
   "
   Syntax
   ======
   (transpose! matrix)

   Purpose
   =======
   Exchange row and column strides so that effectively
   the matrix is transposed in place (without much effort).
"))

(defmethod transpose! ((matrix standard-matrix))
  (rotatef (nrows matrix) (ncols matrix))
  (rotatef (row-stride matrix) (col-stride matrix))
  matrix)

;;
(defgeneric fill-matrix (matrix fill-element)
  (:documentation 
   "
   Syntax
   ======
   (FILL-MATRIX matrix fill-element)
  
   Purpose
   =======
   Fill MATRIX with FILL-ELEMENT.
"))

(defmethod fill-matrix ((matrix t) (fill t))
  (error "arguments MATRIX and FILL to FILL-MATRIX must be a
matrix and a number"))

;;
(defmethod make-load-form ((matrix standard-matrix) &optional env)
  "MAKE-LOAD-FORM allows us to determine a load time value for
   matrices, for example #.(make-matrix ...)"
  (make-load-form-saving-slots matrix :environment env))

;;
#+nil(defmethod print-object ((matrix standard-matrix) stream)
  (dotimes (i (nrows matrix))
    (dotimes (j (ncols matrix))
      (format stream "~A    " (matrix-ref-2d matrix i j)))
    (format stream "~%")))

;;
(defun get-order-stride (matrix &optional (fortran-op "N"))
  (check-type matrix standard-matrix)
  (let ((rs (row-stride matrix))
	(cs (col-stride matrix)))
    (declare (type fixnum rs cs))
    (cond
      ((= cs 1) (values :row-major rs (cond
					((string= fortran-op "N" ) "T")
					((string= fortran-op "T" ) "N"))))
      ((= rs 1) (values :col-major cs (cond
					((string= fortran-op "N" ) "N")
					((string= fortran-op "T" ) "T"))))
      (t nil))))

;;
(defun make-sub-matrix (matrix i j nrows ncols)
  (declare (type standard-matrix matrix)
	   (type fixnum i j nrows ncols))
  (let ((hd (head matrix))
	(nr (nrows matrix))
	(nc (ncols matrix))
	(rs (row-stride matrix))
	(cs (col-stride matrix)))
    (declare (type fixnum hd nr nc rs cs))
    (when (or (> i (- nr nrows)) (> j (- nc ncols)))
      (error "Parent matrix is not big enough to create sub-matrix."))
    (make-instance (class-of matrix)
		   :nrows nrows :ncols ncols
		   :store (store matrix)
		   :head (store-indexing i j hd rs cs)
		   :row-stride rs :col-stride cs)))

;;
(defmacro with-transpose! (matlst &rest body)
  `(progn
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)
     ,@body
     ,@(mapcar #'(lambda (mat) `(transpose! ,mat)) matlst)))

;;
(defun blas-copyable-p (matrix)
  (declare (optimize (safety 0) (speed 3))
	   (type (or real-matrix complex-matrix) matrix))
  (mlet* ((nr (nrows matrix) :type fixnum)
	  (nc (ncols matrix) :type fixnum)
	  (rs (row-stride matrix) :type fixnum)
	  (cs (col-stride matrix) :type fixnum)
	  (ne (number-of-elements matrix) :type fixnum))
	 (cond
	   ((= nc 1) (values t rs ne))
	   ((= nr 1) (values t cs ne))
	   ((= rs (* nc cs)) (values t cs ne))
	   ((= cs (* nr rs)) (values t rs ne))
	   (t (values  nil -1 -1)))))