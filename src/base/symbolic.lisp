(in-package #:matlisp)

(defclass ge-expression ()
  ((expression :initform (error "expression missing") :initarg :expression)
   (inputs :initform nil :initarg :inputs)))

;;Field definitions
(macrolet ((genari (f bf)
	     `(deft/method ,f (ty ge-expression) (&rest nums)
		(with-gensyms (nn)
		  `(let ((,nn (list ,@nums)))
		     (reduce #'(lambda (a b) (reweylify
					      (make-instance 'ge-expression
							     :expression (,',bf (slot-value a 'expression) (slot-value b 'expression))
							     :inputs (union (slot-value a 'inputs) (slot-value b 'inputs) :test #'equal)))) ,nn))))))
  (eval-every
    (genari t/f+ weyl:+)
    (genari t/f- weyl:-)
    (genari t/f* weyl:*)
    (genari t/f/ weyl:/)))

(deft/method t/fid+ (ty ge-expression) () (make-instance 'ge-expression :expression (weyl:zero weyl:*general*)))
(deft/method t/fid* (ty ge-expression) () (make-instance 'ge-expression :expression (weyl:one weyl:*general*)))

(deft/method t/f= (ty ge-expression) (&rest nums)
  `(weyl:= ,@(mapcar #'(lambda (x) `(slot-value ,x 'expression)) nums)))

(deft/method t/fc (ty ge-expression) (num) num)

(defun weylify (expr)
  (let ((flist `((matlisp::tb+ weyl:+) (matlisp::+ weyl:+)
		 (matlisp::tb- weyl:-) (matlisp::- weyl:-)
		 (matlisp::tb*-opt weyl:*) (matlisp::* weyl:*)
		 (matlisp::tb/ weyl:/) (matlisp::/ weyl:/)
		 (matlisp-infix::expt weyl:expt)
		 (matlisp-infix::exp weyl::exp)
		 (matlisp-infix::sin weyl::sin)
		 (matlisp-infix::cos weyl::cos)))
	inputs)
    (values
     (make-instance 'ge-expression
		    :expression 
		    (eval (maptree-if #'(lambda (x) t) #'(lambda (x)
							   (cond
							     ((and (consp x) (assoc (car x) flist)) (values (cons (second (assoc (car x) flist)) (cdr x))
													    #'(lambda (f lst) (cons (car lst) (mapcar f (cdr lst))))))
							     ((symbolp x) (push x inputs) `(weyl:make-ge-variable weyl:*general* (quote ,x)))
							     ((and (consp x) (eql (car x) 'matlisp-infix::generic-ref))
							      (let ((sym (intern (format nil  "~a_~{~a~^,~}" (second x) (cddr x)))))
								(setf inputs (union (list (list sym (cons 'ref (cdr x)))) inputs :test #'equal))
								`(weyl:make-ge-variable weyl:*general* (quote ,sym))))
							     ((consp x) (values x #'(lambda (f lst) (cons (car lst) (mapcar f (cdr lst))))))
							     (t x)))
				      expr))
		    :inputs inputs))))

(defun reweylify (expr)
  (let* ((rexpr (weylify (weyli:lispify (slot-value expr 'expression)))))
    (setf (slot-value rexpr 'inputs) (mapcar #'(lambda (zz) (or (find-if #'(lambda (x) (and (consp x) (eql (car x) zz))) (slot-value expr 'inputs)) zz)) (slot-value rexpr 'inputs)))
    rexpr))

(deft/method t/coerce (ty ge-expression) (num)
  (with-gensyms (nn)
    `(let ((,nn ,num))
       (if (typep ,nn 'ge-expression) ,nn
	   (progn
	     (assert (typep ,nn '(or real list symbol)) nil "don't know how to coerce ~a into ge-expression" ,nn)
	     (weylify ,nn))))))

(deft/method (t/store-allocator #'linear-storep) (sym #.(tensor 'ge-expression)) (size &optional initial-element)
  (with-gensyms (sitm size-sym arr idx init)
    (let ((type (second (store-type sym))))
      `(let*-typed ((,size-sym (t/compute-store-size ,sym (let ((,sitm ,size))
							    (etypecase ,sitm
							      (index-type ,sitm)
							      (index-store-vector (lvec-foldr #'* (the index-store-vector ,sitm)))
							      (cons (reduce #'* ,sitm))))))
		    ,@(when initial-element `((,init ,initial-element :type ,(field-type sym))))
		    (,arr (make-array ,size-sym :element-type ',type :initial-element (t/fid+ ,type)) :type ,(store-type sym)))
	 ,@(when initial-element
	     `((very-quickly (loop :for ,idx :from 0 :below ,size-sym :do (t/store-set ,sym ,init ,arr ,idx)))))
	 ,arr))))

(defmethod print-object ((obj ge-expression) stream)
  (format stream "~a" (slot-value obj 'expression)))
;;

#+nil
(let ((a (zeros 2 '(ge-expression))))
  (setf (ref a 0) '#i(sin (x [0])))
  a)

(defun symbolify! (x tensor)
  (iter (for-mod idx from 0 below (dimensions tensor) with-iterator ((:stride ((of (strides tensor) (head tensor))))))
	(setf (store-ref tensor of) (weylify (intern (format nil  "~a_~{~a~^,~}" x (coerce idx 'list))))))
  tensor)

(defun deriv (f x)
  (declare (symbol x))
  (etypecase f
    (#.(tensor 'ge-expression) 
       (let* ((nd (iter outer (for-mod idx from 0 below (dimensions f) with-iterator ((:stride ((of-f (strides f) (head f))))))
			(with xd = nil)
			(offset-ref ((ref-f of-f f :type #.(tensor 'ge-expression)))
				    (iter (for ii in (slot-value ref-f 'inputs))
					  (let ((refp (and (consp ii) (eql (second (second ii)) x)))
						(eqlp (or (eql ii x) (and (consp ii) (eql (first ii) x)))))
					    (etypecase xd
					      (null (cond (eqlp (setf xd 0)) (refp (setf xd (coerce (cddr (second ii)) 'index-store-vector)))))
					      (integer (assert (not refp) nil "error: inconsistency in input dimensions: ~a" x))
					      (index-store-vector
					       (assert (not eqlp) nil "error: inconsistency in input dimensions: ~a" x)
					       (when refp
						 (iter (for i from 0 below (length xd))
						       (for si in (cddr (second ii)))
						       (setf (aref xd i) (max (aref xd i) si))
						       (finally (assert (= i (length xd)) nil )))
						 (assert (= (length (cddr (second ii)))) nil "error: inconsistency in input dimensions: ~a" x)))))))
			(finally (return-from outer (etypecase xd (integer (1+ xd)) (index-store-vector (map 'list #'1+ xd)))))))
	      (d (zeros (append (dimensions f t) (ensure-list nd)) '(ge-expression)))
	      (v-d (subtensor~ d (append (make-list (order f) :initial-element 0) (make-list (length (ensure-list nd)) :initial-element (list 0 nil))))))
	 (iter (for-mod lidx from 0 below (dimensions f) with-iterator ((:stride ((of-f (strides f) (head f))
										  (of-d (subseq (strides d) 0 (order f)) (head d))))))
	       (iter (for-mod idx from 0 below (dimensions v-d) with-iterator ((:stride ((of-vd (strides v-d) 0)))))
		     (setf (store-ref v-d (+ of-vd of-d))
			   (let ((expr (store-ref f of-f)))
			     (reweylify
			      (make-instance 'ge-expression
					     :expression
					     (weyl:deriv (slot-value expr 'expression) (if (integerp nd) x  (intern (format nil  "~a_~{~a~^,~}" x (coerce idx 'list)))))
					     :inputs (slot-value expr 'inputs)))))))
	 (values d nd)))
    (ge-expression
     (letv* ((d nd (deriv (copy (list f) (tensor 'ge-expression)) x)))
       (if (integerp nd) (ref d 0 0) (orphanize (slice~ d 0)))))
    (t (deriv (t/coerce ge-expression f) x))))

#+nil
(definline ge-expressiond.diff (a x)
  (etypecase a
    (ge-expression
     (maxima::$diff a x))
    (symbolic-tensor
     (make-instance 'symbolic-tensor
		    :dimensions (copy-seq (dimensions a))
		    :store (map 'symbolic-store-vector #'(lambda (f) (maxima::$diff f x)) (store a))))))
