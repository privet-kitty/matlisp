(in-package #:matlisp-fibonacci)

(defstruct hnode
  (id -1 :type fixnum)
  (degree 0 :type fixnum)
  (mark? nil :type boolean)
  (parent nil :type (or null hnode))
  (children nil :type list)
  (dcons nil :type list)
  key)

(definline dappend2 (a b)
  (declare (type list a b))
  (cond
    ((null a) b) ((null b) a)
    (t (rotatef (first a) (first b))
       (rotatef (second (first a)) (second (first b)))
       a)))

(closer-mop:defmethod print-object ((nd hnode) stream)
  (print-unreadable-object (nd stream :type t)
    (format stream "key: ~A, degree: ~A, mark?: ~A" (hnode-key nd) (hnode-degree nd) (hnode-mark? nd))))
;;
(closer-mop:defclass fib-heap ()
  ((root :initform nil :accessor root)
   (number-of-trees :initform 0 :accessor number-of-trees)
   (number-of-elements :initform 0 :accessor number-of-elements)
   (heap-order :initarg :heap-order :initform #'<)
   (node-table :initarg :node-table :initform (make-hash-table) :accessor node-table)))

(closer-mop:defmethod matlisp::total-size ((obj fib-heap)) (slot-value obj 'number-of-elements))

(closer-mop:defmethod print-object ((fib fib-heap) stream)
  (print-unreadable-object (fib stream :type t)
    (format stream "size: ~A, trees: ~A" (number-of-elements fib) (number-of-trees fib))))

(defun make-heap (&optional (func #'<) (test 'eql))
  (make-instance 'fib-heap :heap-order func :node-table (make-hash-table :test test)))

(definline fib-order (a b fib)
  (funcall (slot-value fib 'heap-order) a b))

(definline min-key (fib)
  (when (root fib)
    (values (hnode-key (dcar (root fib))) (hnode-id (dcar (root fib))))))

(defun insert-key (key fib &optional id)
  (letv* ((pmin mid (min-key fib))
	  (node (make-hnode :key key :id (or id (number-of-elements fib)))))
    (assert (not (node-existsp (hnode-id node) fib)) nil "id already exists in the heap")
    (setf (hnode-dcons node) (dpush node (root fib)))
    (unless (and mid (fib-order key pmin fib))
      (setf (root fib) (dcdr (root fib))))
    (setf (gethash (hnode-id node) (node-table fib)) node)
    (incf (number-of-elements fib))
    (incf (number-of-trees fib))
    (hnode-id node)))

(definline node-existsp (id fib)
  (letv* ((node exists-p (gethash id (node-table fib))))
    (and exists-p (hnode-dcons node) t)))

(definline node-key (id fib)
  (letv* ((node exists-p (gethash id (node-table fib))))
    (when exists-p (hnode-key node))))

(definline (setf node-key) (value id fib)
  (decrease-key id value fib))

(defun extract-min (fib)
  (let ((z (dpop (root fib))))
    ;;move the children of z into root
    (when z
      (decf (number-of-elements fib))
      (decf (number-of-trees fib))
      (iter (for node in-dlist (hnode-children z))
	    (counting t into i)
	    (setf (hnode-parent node) nil)
	    (finally (incf (number-of-trees fib) i)))
      (setf (root fib) (dappend2 (hnode-children z) (root fib)))
      (setf (hnode-children z) nil
	    (hnode-degree z) 0
	    (hnode-dcons z) nil))
    ;;
    (when (> (number-of-trees fib) 1)
      ;;consolidate
      (let ((an (make-array (+ 2 (integer-length (number-of-elements fib))) :initial-element nil)))
	(iter (for w on-dlist (root fib))
	      (with fw = (list t t))
	      ;;This hack allows for destructive updates. See the iterate defclause in dlist.lisp
	      (setf (second fw) (second w))
	      (iter (with x = w)
		    (with d = (hnode-degree (dcar x)))
		    (while (aref an d))
		    ;;
		    (let ((y (aref an d)))
		      (when (fib-order (hnode-key (dcar y)) (hnode-key (dcar x)) fib)
			(rotatef y x))
		      ;;fib-heap-link
		      (let ((y.node (dpop y)))
			(setf (root fib) y
			      (hnode-parent y.node) (dcar x)
			      (hnode-mark? y.node) nil)
			(decf (number-of-trees fib))
			(setf (hnode-children (dcar x)) (dappend2 (hnode-dcons y.node) (hnode-children (dcar x))))))
		    (setf (aref an d) nil)
		    ;;
		    (incf d)
		    (setf (hnode-degree (dcar x)) d)
		    ;;
		    (finally (setf (aref an d) x)))
	      (setq w fw))
	;;update min
	(iter (for rot on-dlist (root fib))
	      (with fmin = nil)
	      (when (or (null fmin) (fib-order (hnode-key (dcar rot)) (hnode-key (dcar fmin)) fib))
		(setf fmin rot))
	      (finally (setf (root fib) fmin)))))
    ;;
    (when z
      (values (hnode-key z) (hnode-id z)))))

;;
(definline cut (x y fib)
  "cut T_x from δ(y)"
  (decf (hnode-degree y))
  (setf (hnode-children y) (let ((tmp (hnode-dcons x))) (dpop tmp) tmp)
	(hnode-parent x) nil
	(hnode-mark? x) nil)
  (incf (number-of-trees fib))
  (dappend2 (hnode-dcons x) (root fib)))

(defun ccut (y fib)
  "cascading cut"
  (when-let ((z (hnode-parent y)))
    (if (hnode-mark? y)
	(progn (cut y z fib) (ccut z fib))
	(setf (hnode-mark? y) t))))

(defun decrease-key (id key fib)
  (let ((node (gethash id (node-table fib))))
    (declare (type hnode node))
    (assert (not (fib-order (hnode-key node) key fib)) nil 'invalid-value :message "new key is greater than the current.")
    (setf (hnode-key node) key)
    (when (hnode-dcons node)
      ;;cut node
      (let ((y (hnode-parent node)))
	(when (and y (fib-order key (hnode-key y) fib))
	  (cut node y fib) (ccut y fib)))
      ;;update min
      (when (fib-order key (min-key fib) fib)
	(setf (root fib) (hnode-dcons node)))))
  key)

(defun delete-node (id fib &optional delete?)
  (let ((node (gethash id (node-table fib))))
    (declare (type hnode node))
    (when (hnode-dcons node)
      ;;cut node
      (let ((y (hnode-parent node)))
	(when y (cut node y fib) (ccut y fib)))
      ;;move to root
      (setf (root fib) (hnode-dcons node))
      (extract-min fib))
    (when delete? (remhash id (node-table fib))))
  id)
;;

;; (let ((fib (make-instance 'fib-heap)))
;;   (fib-insert 0 fib)
;;   (fib-extract-min fib))


;; (let ((fib (make-instance 'fib-heap)))
;;   (loop :for i from 0 :below 100 :do (insert-key i fib))
;;   (loop :for i from 0 :below 100 :do (let ((min (extract-min fib)))
;; 				       (unless (eql i min) (format t "noo! ~a" min)))))
