(in-package #:matlisp)

;;These describe some global defaults governing the
;;runtime behavior of Matlisp. Although you can change
;;these by doing a setf during runtime, it is recommended
;;that you use lexical scoping to affect local changes to
;;code (global variables are only bad if you overwrite them :)

(define-constant +infinity+
  (matlisp-ffi::with-fortran-float-modes
    ;;(ieee-floats:decode-float64 #x7ff0000000000000)
    (/ 1d0 0d0))
  :test '=)

;;
(defparameter *sparse-tensor-realloc-on-setf* nil)

(defparameter *default-sparse-store-increment* 100
  "
  Determines the increment by which the store of a compressed sparse matrix is increased,
  when it runs out of store.")

(defparameter *default-sparsity* 1/1000
  "
  Determines the default sparsity for a newly created sparse matrix, when the number of non-zero is
  not specified.")

(defparameter *max-sparse-size* 10000
  "
  Upper bounds the store size for a newly created sparse matrix, when the number of non-zero is
  not specified.")

;;Default ordering of strides
(defparameter *default-stride-ordering* :col-major
  "
  Determines whether strides are row or column major by default.
  Doing:
  > (let ((*default-stride-ordering* :col-major))
      (make-real-tensor 10 10))
  returns a 10x10 matrix with Column major order.
")

(defparameter *default-tensor-type* '(double-float))

(defparameter *check-after-initializing?* t
  "
  If t, then check for invalid values in the field of
  the class in the :after specialized method (if defined),
  else do nothing. One ought to be very carful when doing,
  much of Matlisp's code is written on the assumption that
  the fields of a tensor don't take invalid values; failing
  which case, may lead to memory error. Use at your own risk.
")

(defmacro with-no-init-checks (&rest body)
  `(let ((*check-after-initializing?* nil))
     ,@body))

(defparameter *rcond-scale* 10
  "
  Factor by which the float-epsilon is to be scaled, so as to obtain a condition number threshold,
  to be used for determining the rank of a matrix (used in gelsy).
")

(defparameter *default-uplo* :l
  "
  For routines which take symmetric (hermitian) matrices as
  arguments, this sets the default argument for UPLO.")
;;Level 1--------------------------------------------------------;;
(defparameter *real-l1-fcall-lb* 5000
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads.
   The Fortran function is not called if the tensor does not have
   a consecutive store (see blas-helpers.lisp/consecutive-store-p).")

(defparameter *complex-l1-fcall-lb* 2500
  "If the size of the array is less than this parameter, the
   lisp version of axpy is called in order to avoid FFI overheads.
   The Fortran function is not called if the tensor does not have
   a consecutive store (see blas-helpers.lisp/consecutive-store-p).")

;;Level 2--------------------------------------------------------;;
(defparameter *real-l2-fcall-lb* 1000
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices. Note that if the dimensions do exceed
  this lower  bound, then the Fortran function is called even if
  the matrix has a BLAS incompatible stride (by doing a copy).

  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 800 and 2000.")

(defparameter *complex-l2-fcall-lb* 600
  "
  If the maximum dimension in the MV is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices. Note that if the dimensions do exceed
  this lower bound, then the Fortran function is called even when
  the matrices have a BLAS incompatible stride (by using a copy).

  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 400 and 1000.")
;;Level 3--------------------------------------------------------;;
(defparameter *real-l3-fcall-lb* 50
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")

(defparameter *complex-l3-fcall-lb* 30
  "
  If the maximum dimension in the MM is lower than this
  parameter, then the lisp code is used by default, instead of
  calling BLAS. Used to avoid the FFI overhead when calling
  MM with small matrices.
  Default set with SBCL on x86-64 linux. A reasonable value
  is something between 20 and 200.")
;;
(defparameter *dbg* nil)
