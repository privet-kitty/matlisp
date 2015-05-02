(in-package #:matlisp)

;;This will only work if type is a dense-tensor
(defun ones (dims &optional (type *default-tensor-type*))
  (zeros dims type 1))

(defun eye! (tensor)
  (tricopy! 1 (copy! 0 tensor) :d))

(defun eye (dims &optional (type *default-tensor-type*))
  (tricopy! 1 (zeros dims type) :d))

(defun diag (tensor &optional (order 2))
  (declare (type (and tensor-vector dense-tensor) tensor))
  (tricopy! tensor (zeros (make-list order :initial-element (dimensions tensor 0)) (type-of tensor)) :d))

(defun diag~ (a)
  (declare (type dense-tensor a))
  (with-no-init-checks
    (make-instance (class-of a)				
      :dimensions (coerce (list (lvec-min (dimensions a))) 'index-store-vector)
      :strides (coerce (list (lvec-foldr #'+ (strides a))) 'index-store-vector)
      :head (head a) :store (store a) :parent a)))

(defun (setf diag~) (value tensor) (copy! value (diag~ tensor)))
