(in-package #:matlisp)
(in-readtable :infix-dispatch-table)

(defclass base-accessor ()
  ((dimensions :initarg :dimensions :type index-store-vector
		:documentation "Dimensions of the vector spaces in which the tensor's arguments reside.")))

;;Vanilla store
(defclass stride-accessor (base-accessor)
  ((strides :initarg :strides :type index-store-vector
	    :documentation "Strides for accesing elements of the tensor.")
   (head :initarg :head :initform 0 :reader head :type index-type
	 :documentation "Head for the store's accessor.")))

;;Bipartite/Factor/Co-ordinate store
(defclass coordinate-accessor (base-accessor)
  ((indices :initarg :indices :type index-store-matrix
	    :documentation "Non-zero indices in the tensor.")))

;;Graph store, only works for matrices.
(defclass graph-accessor (base-accessor)
  ((fence :initarg :fence :type index-store-vector :documentation "Start index for neighbourhood.")
   (neighbours :initarg :neighbours :type index-store-vector :documentation "Neighbour id.")))

(definline v-i (g i)
  (declare (type index-type i)
	   (type graph-accessor g))
  (let-typed ((f (slot-value g 'fence) :type index-store-vector))
    (values (aref f i) (aref f (1+ i)))))

(definline |δ-I| (g i)
  (letv* ((l r (v-i g i))) (- r l)))

(definline δ-i (g i &optional j)
  (declare (type index-type i)
	   (type graph-accessor g))
  (let ((fe (slot-value g 'fence)) (ag (slot-value g 'neighbours)))
    (declare (type index-store-vector fe ag))
    (if j
	(progn (assert (< -1 j (|δ-I| g i)) nil 'tensor-index-out-of-bounds)
	       (aref ag (+ (aref fe i) j)))
	(iter (for j from (aref fe i) below (aref fe (1+ i))) (collect (aref ag j))))))

;;
(defun graph-queue (init g)
  (let* ((queue (make-fib #'(λ (a b) (let ((a (cdr a)) (b (cdr b)))
				       (if (and a b) (< a b) (and a t))))))
	 (|v| (1- (length (neighbour-start g))))
	 (vv (make-array |v|)))
    (iter (for i from 0 below |v|)
	  (setf (aref vv i) (fib-insert (cons i (funcall init g i)) queue)))
    (values queue vv)))

;; (defun fill-in (i g)
;;   (let* ((v (neighbour-start g)) (ag (neighbour-id g))
;; 	 (δ (δ-n i g)))
;;     (declare (type index-store-vector v ag))
;;     (iter (for u in-vector ag from (aref v i) below (aref v (1+ i)))
;; 	  )

#'(λ (g i) (|δ-I| g i))

(defun )

(defun graph-pop (init update g)
  (letv* ((fe (slot-value g 'fence)) (ag (slot-value g 'neighbours))
	  (queue nodes (graph-queue init)))
    (declare (type index-store-vector fe ag))
    (iter (repeat (1- (length fe)))
	  (letv* (((i . wi) (fib-extract-min q)))
	    (iter (for j from (aref v i) below (aref v (1+ i)))
		  (let* ((u (aref ag j)) (nu (aref n u))
			 (wu ))))
	    (funcall update g i wδ)
	    (let* ((u (aref ag j)))
		  (funcall update i g)
		  (let ((d.us (cdr (hnode-key u.node))))
		    (when (or (not d.us) (< (+ wu d) d.us))
		      (fib-decrease-key u.node (cons u (+ wu d)) queue))))))
    ))
