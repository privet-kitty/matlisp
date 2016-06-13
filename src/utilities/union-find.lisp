(in-package #:matlisp-union-find)

;;
(closer-mop:defclass union-find ()
  ((id :initform (make-array 0 :adjustable t :fill-pointer t))
   (values :initarg :values :initform (make-array 0 :adjustable t :fill-pointer t))))

;(closer-mop:defmethod matlisp::total-size ((obj union-find)) (length (slot-value obj 'id)))

(closer-mop:defmethod print-object ((obj union-find) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size: ~A" (length (slot-value obj 'id)))))

(closer-mop:defmethod initialize-instance :after ((obj union-find) &rest initargs)
  (declare (ignore initargs))
  (when (listp (slot-value obj 'values))
    (let ((vv (slot-value obj 'values)))
      (setf (slot-value obj 'values) (make-array 0 :adjustable t :fill-pointer t))
      (iter (for vi in vv) (insert-item vi obj)))))

(defun insert-item (item ufd)
  (let ((id (slot-value ufd 'id)))
    (vector-push-extend (length id) id))
  (vector-push-extend item (slot-value ufd 'values))
  item)

(defun root (i ufd)
  (let ((id (slot-value ufd 'id)))
    (iter (for ui initially i then (aref id ui))
	  (if (= ui (aref id ui)) (return ui))
	  (setf (aref id ui) (aref id (aref id ui))))))

(defun find-items (i j ufd)
  (= (root i ufd) (root j ufd)))

(defun unite-items (i j ufd)
  (let ((rj (root j ufd)) (id (slot-value ufd 'id)))
    (iter (with ui = i)
	  (let ((ui+ (aref id ui)))
	    (setf (aref id ui) rj)
	    (if (= ui ui+) (return ui) (setf ui ui+)))))
  ufd)
;;ಕರ್ಮ!
