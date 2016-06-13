(in-package #:matlisp)

;;Field definitions
(eval-every
  (closer-mop:defclass ge-expression ()
    ((expression :initform (error "expression missing") :initarg :expression)
     (inputs :initform nil :initarg :inputs)))

  (tensor 'ge-expression)
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

  (deft/method t/fc (ty ge-expression) (num) num))

(defparameter *function-assoc-table*
  `(,@(mapcar #'(lambda (x) `(,x ,(find-symbol (symbol-name x) :weyl)))
	      `(cl:+ cl:- cl:* cl:/ t::+ t::- t::* t::/
		     cl:expt t:expt cl:exp t:exp cl:log t:log t:sin cl:sin t:asin cl:asin t:cos cl:cos t:acos cl:acos
		     t:tan cl:tan t:atan cl:atan
		     t:sinh cl:sinh t:cosh cl:cosh t:tanh cl:tanh t:asinh cl:asinh
		     t:acosh cl:acosh t:atanh cl:atanh))))

(defun weylify (expr)
  (let ((flist *function-assoc-table*)
	inputs)
    (values
     (make-instance 'ge-expression
		    :expression
		    (eval (maptree-if #'(lambda (x) t) #'(lambda (x)
							   (match x
							     ((list* (guard fname (assoc fname flist)) rest)
							      (values (list* (second (assoc fname flist)) rest)
								      #'(lambda (f lst) (list* (car lst) (mapcar f (cdr lst))))))
							     ((symbol) (setf inputs (adjoin x inputs :test #'equal))
							      `(weyl:make-ge-variable weyl:*general* (quote ,x)))
							     ((list* 'matlisp-infix::generic-ref tensor subscripts)
							      (let ((sym (intern (format nil  "~a_~{~a~^,~}" tensor subscripts))))
								(setf inputs (union (list (list sym (cons 'ref (cdr x)))) inputs :test #'equal))
								`(weyl:make-ge-variable weyl:*general* (quote ,sym))))
							     ((type cons) (values x #'(lambda (f lst) (cons (car lst) (mapcar f (cdr lst))))))
							     (_ x)))
				      expr))
		    :inputs inputs))))

(deft/method t/coerce (ty ge-expression) (num)
  (with-gensyms (nn)
    `(let ((,nn ,num))
       (if (typep ,nn 'ge-expression) ,nn
	   (progn
	     (assert (typep ,nn '(or real list symbol)) nil "don't know how to coerce ~a into ge-expression" ,nn)
	     (weylify ,nn))))))

(deft/method (t/store-allocator #'linear-storep) (sym #.(tensor 'ge-expression)) (size &rest initargs)
  (letv* (((&key initial-element) initargs))
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
	   ,arr)))))

(defun reweylify (expr)
  (let* ((rexpr (weylify (weyli::lispify (slot-value expr 'expression)))))
    (setf (slot-value rexpr 'inputs) (mapcar #'(lambda (zz) (or (find-if #'(lambda (x) (and (consp x) (eql (car x) zz))) (slot-value expr 'inputs)) zz)) (slot-value rexpr 'inputs)))
    rexpr))

(defmethod print-object ((obj ge-expression) stream)
  (format stream "~a" (slot-value obj 'expression)))
;;

#+nil
(let ((a (zeros 2 '(ge-expression))))
  (setf (ref a 0) '#i(sin (x [0])))
  a)

(defun symbolify! (x tensor)
  (iter (for-mod idx from 0 below (dimensions tensor) with-iterator ((:stride ((of (strides tensor) (head tensor))))))
	(let* ((sym (intern (format nil  "~a_~{~a~^,~}" x (coerce idx 'list)))) (expr (weylify sym)))
	  (setf (slot-value expr 'inputs) `((,sym (ref ,x ,@(coerce idx 'list))))
		(store-ref tensor of) expr)))
  tensor)

#+nil
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


(defun deriv (f x)
  (labels ((sderiv! (f x df)
	     (etypecase x
	       (#.(tensor 'ge-expression)
		  (dorefs (idx (dimensions x))
		    ((ref-x x :type #.(tensor 'ge-expression)) (ref-df df :type #.(tensor 'ge-expression)))
		    (setf ref-df (sderiv! f ref-x nil)))
		  df)
	       ((or ge-expression symbol)
		(reweylify
		 (make-instance 'ge-expression
				:expression (weyl:deriv (slot-value f 'expression) (if (typep x 'ge-expression) (slot-value x 'expression) x))
				:inputs (slot-value f 'inputs)))))))
    (etypecase f
      (#.(tensor 'ge-expression)
	 (letv* ((vx dim.x (if (typep x '#.(tensor 'ge-expression)) (values x (dimensions x t)) (values (copy `(,x) (tensor 'ge-expression)) (list 1))))
		 (df (zeros (append (dimensions f t) dim.x) '(ge-expression)))
		 (v.df (subtensor~ df (append (make-list (order f) :initial-element 0) (make-list (length dim.x) :initial-element (list 0 nil))))))
	   (iter (for-mod lidx from 0 below (dimensions f) with-iterator
			  ((:stride ((of-f (strides f) (head f))
				     (of-df (subseq (strides df) 0 (order f)) (head df))))))
		 (setf (slot-value v.df 'head) of-df)
		 (sderiv! (store-ref f of-f) vx v.df))
	   df))
      (ge-expression
       (sderiv! f x (if (typep x '#.(tensor 'ge-expression)) (zeros (dimensions x) (type-of x)))))
      (t (deriv (t/coerce ge-expression f) x)))))

#+nil
(definline ge-expressiond.diff (a x)
  (etypecase a
    (ge-expression
     (maxima::$diff a x))
    (symbolic-tensor
     (make-instance 'symbolic-tensor
		    :dimensions (copy-seq (dimensions a))
		    :store (map 'symbolic-store-vector #'(lambda (f) (maxima::$diff f x)) (store a))))))

(defmacro compile-symbolic ((type) expr)
  (let ((inputs nil))
    (labels ((compiler (expr &optional place)
	       (etypecase expr
		 (#.(tensor 'ge-expression)
		    (with-gensyms (ret sto hd std)
		      `(let*-typed ((,ret ,(or place `(zeros (list ,@(dimensions expr t)) ',(tensor type))) :type ,(tensor type))
				    (,sto (store ,ret) :type ,(store-type (tensor type))) (,hd (head ,ret) :type index-type) (,std (strides ,ret) :type index-store-vector))
			 ,@(iter (for-mod idx from 0 below (dimensions expr))
				 (let* ((subscripts (coerce idx 'list)))
				   (collect `(setf (t/store-ref ,(tensor type) ,sto
								(the index-type (+ (the index-type ,hd)
										   ,@(mapcar #'(lambda (x ii) `(the index-type (* (aref ,std ,ii) (the index-type ,x))))
											     subscripts (range 0 (length subscripts) nil t)))))
						   ,(compiler (apply #'ref (list* expr subscripts)))))))
			 ,ret)))
		 (ge-expression
		  (setf inputs (union inputs (slot-value expr 'inputs) :test #'equal))
		  (maptree-eki #'(lambda (x)
				   (trivia:match x
				     ((trivia:guard x (numberp x)) (coerce x type))
				     ((list 'expt m e) x)
				     (_ (values x t))))
			       (weyli::lispify (slot-value expr 'expression)))))))
      (let ((kern (maptree-eki #'(lambda (x) (if (typep x `(or ge-expression ,(tensor 'ge-expression))) (compiler x) (values x t))) (eval expr))))
	`(let (,@(remove-if-not #'consp inputs)) ,kern)))))
