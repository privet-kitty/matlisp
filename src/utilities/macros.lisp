(in-package #:matlisp-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;Note to self: do not indent!v

(defmacro using-gensyms ((decl (&rest syms) &optional gensyms) &body body)
  `(let ((,decl (zip ',(mapcar #'(lambda (x) (gensym (symbol-name x))) syms) (list ,@syms))))
     (destructuring-bind (,@syms) (mapcar #'car ,decl)
       ,(append
	 (if gensyms
	   `(with-gensyms (,@gensyms)) `(progn))
	 body))))

(defmacro binding-gensyms ((mname &optional (fname (gensym))) &body body)
  `(with-memoization ()
     (memoizing
      (flet ((,fname (x) (gensym (symbol-name x))))
	(macrolet ((,mname (x) `(,', fname ',x)))
	  ,@body)))))

(defmacro ziprm ((r m) &body args)
  "
  Does reduce-map on @arg{args}.

  Example:
  @lisp
  > (macroexpand-1
       `(ziprm (and =) (a b c) (1 2 3)))
  => (AND (= A 1) (= B 2) (= C 3))
  @end lisp
  "
  `(,r ,@(apply #'mapcar #'(lambda (&rest atoms) (cons m atoms)) (mapcar #'ensure-list args))))

(defmacro inline-member (x lst &optional (test 'eql))
  (with-gensyms (xx) `(let ((,xx ,x)) (or ,@(mapcar #'(lambda (l) `(,test ,xx ,l)) lst)))))
;;
(defmacro cart-case ((&rest vars) &body cases)
  (let ((decl (zipsym vars)))
    `(let (,@decl)
       (cond ,@(mapcar #'(lambda (clause) `((and ,@(mapcar #'(lambda (x)
							       (if (consp (second x))
								   `(or ,@(mapcar #'(lambda (u) `(eql ,(first x) (quote ,u))) (second x)))
								   `(eql ,(first x) (quote ,(second x)))))
							   (remove t (zip (mapcar #'car decl) (first clause)) :key #'second))) ,@(cdr clause))) cases)))))

(defmacro cart-ecase ((&rest vars) &body cases)
  (let ((decl (zipsym vars)))
    `(let (,@decl)
       (cond ,@(mapcar #'(lambda (clause) `((ziprm (and eql) ,(mapcar #'car decl) ,(first clause)) ,@(cdr clause))) cases)
	 (t (error "cart-ecase: Case failure."))))))

(defmacro cart-typecase (vars &body cases)
  (let* ((decl (zipsym vars)))
    `(let (,@decl)
       (cond ,@(mapcar #'(lambda (clause) `((ziprm (and typep) ,(mapcar #'car decl) ,(mapcar #'(lambda (x) `(quote ,x)) (first clause))) ,@(cdr clause))) cases)))))

(defmacro cart-etypecase (vars &body cases)
  (let* ((decl (zipsym vars)))
    `(let (,@decl)
       (cond ,@(mapcar #'(lambda (clause)
			   `((ziprm (and typep) ,(mapcar #'car decl) ,(mapcar #'(lambda (x) `(quote ,x)) (first clause)))
			     (locally (declare ,@(mapcar #'(lambda (x y) `(type ,x ,y)) (first clause) (mapcar #'car decl))) ,@(cdr clause))))
		       cases)
	     (t (error "cart-etypecase: Case failure."))))))
;;
(defmacro values-n (n &rest values)
  (using-gensyms (decl (n))
    (labels ((make-cd (i rets vrets)
	       `((let ((,(first (car rets)) ,(maptree '(values-n :previous) #'(lambda (x) (match x
											    ((list* 'values-n _) x)
											    ((λlist :previous &optional (idx (- i 2)))
											     (assert (< -1 idx (length vrets)) nil 'invalid-arguments)
											     (elt (reverse vrets) idx))))
						      (second (car rets)))))
		   ,(recursive-append
		     (when (cdr rets)
		       `(if (> ,n ,i) ,@(make-cd (1+ i) (cdr rets) (cons (caar rets) vrets))))
		     `(values ,@(reverse vrets) ,(caar rets)))))))
      `(let (,@decl)
	 (when (> ,n 0)
	   ,@(make-cd 1 (zipsym values) nil))))))

(defmacro letv* (bindings &body body)
  "
  This macro extends the syntax of let* to handle multiple values and destructuring bind,
  it also handles type declarations. The declarations list @arg{vars} is similar to that in let:
  look at the below examples.

  Examples:
  @lisp
  > (macroexpand-1 `(letv* ((x 2 :type fixnum)
			    ((a &optional (c 2)) b (values (list 1) 3) :type (fixnum &optional (t)) t))
		      t))
  => (LET ((X 2))
	   (DECLARE (TYPE FIXNUM X))
       (MULTIPLE-VALUE-BIND (#:G1120 B) (VALUES (LIST 1) 3)
	 (DECLARE (TYPE T B))
	 (DESTRUCTURING-BIND (A &OPTIONAL (C 2)) #:G1120
	   (DECLARE (TYPE FIXNUM A)
		    (TYPE T C))
	   (LOCALLY T))))
  @end lisp
  "
  (let ((consy (gensym "consy")))
    (labels ((typedecl (syms alist)
	       (let ((decls (remove-if #'null (mapcar #'(lambda (s)
							  (let ((ts (assoc s alist)))
							    (when ts
							      (if (second ts)
								  `(type ,(second ts) ,s)
								  `(ignore ,s)))))
						      syms))))
		 (when decls `((declare ,@decls))))))
      (apply #'recursive-append
	     (append
	      (mapcan #'(lambda (x)
			  (destructuring-bind (bind expr type) (let ((tpos (position :type x)) (len (length x)))
								 (list (deconsify (subseq x 0 (1- (or tpos len))) consy) (nth (1- (or tpos len)) x) (when tpos (deconsify (nthcdr (1+ tpos) x) consy))))
			    #+nil(let ((flt (remove consy (flatten bind))))
			      (assert (= (length flt) (length (remove-duplicates flt))) nil "Duplicates present in binding ~a" flt))
			    (let* ((typa (maptree t #'(lambda (x) (if (atom (car x))
								      (unless (or (eql (car x) consy) (member (car x) cl:lambda-list-keywords)) (list x))
								      (values x #'(lambda (mf x) (apply #'append (mapcar mf x))))))
						  (ziptree bind type)))
				   (vsyms (mapcar #'(lambda (x) (if (listp x)
								    (let ((g (gensym)))
								      (list g `(destructuring-bind ,(reconsify x consy) ,g ,@(typedecl (flatten x) typa))))
								    (list x)))
						  bind)))
			      (list*
			       (recursive-append
				(if (> (length bind) 1)
				    `(multiple-value-bind (,@(mapcar #'car vsyms)) ,expr)
				    `(let ((,@(mapcar #'car vsyms) ,expr))))
				(car (typedecl (mapcar #'car vsyms) typa)))
			       (remove-if #'null (mapcar #'cadr vsyms))))))
		      bindings)
	      `((locally ,@body)))))))

(defmacro let-typed (bindings &body body)
  "
  This macro works basically like let, but also allows type-declarations
  with the key :type.

  Example:
  @lisp
  > (macroexpand-1
      `(let-typed ((x 1 :type fixnum))
	  (+ 1 x)))
  => (LET ((X 1))
	(DECLARE (TYPE FIXNUM X))
	(+ 1 X))
  @end lisp
  "
  (destructuring-bind (decl body) (trivia:match (first body)
				    ((cons 'declare _) (list (first body) (rest body)))
				    (_ (list nil body)))

    `(let (,@(mapcar #'(lambda (x) (subseq x 0 2)) bindings))
       ,@(let ((types (remove-if #'null (mapcar #'(lambda (x) (destructuring-bind (s e &key (type t)) x
								(declare (ignore e))
								(unless (eql type t)
								  (if (null type)
								      `(ignore ,s)
								      `(type ,type ,s)))))
						bindings))))
	      (when types `((declare ,@types ,@(cdr decl)))))
       ,@body)))

(defmacro let*-typed (bindings &body body)
  "
  This macro works basically like let*, but also allows type-declarations
  with the key :type.

  Example:
  @lisp
  > (macroexpand-1
      `(let*-typed ((x 1 :type fixnum))
	  (+ 1 x)))
  => (LET* ((X 1))
	(DECLARE (TYPE FIXNUM X))
	(+ 1 X))
  @end lisp
  "
  (destructuring-bind (decl body) (trivia:match (first body)
				    ((cons 'declare _) (list (first body) (rest body)))
				    (_ (list nil body)))
    `(let* (,@(mapcar #'(lambda (x) (subseq x 0 2)) bindings))
       ,@(let ((types (remove-if #'null
				 (mapcar #'(lambda (x) (destructuring-bind (s e &key (type t)) x
							 (declare (ignore e))
							 (unless (eql type t)
							   (if (null type)
							       `(ignore ,s)
							       `(type ,type ,s)))))
					 bindings))))
	      (when types `((declare ,@types ,@(cdr decl)))))
       ,@body)))

(defmacro if-ret (form &body else-body)
  "
  If @arg{form} evaluates to non-nil, it is returned, else
  the s-expression @arg{else-body} is evaluated.

  Example:
  @lisp
  > (macroexpand-1
      `(if-ret (when (evenp x) x)
	     (+ x 1)))
  => (LET ((#:G927 (WHEN (EVENP X) X)))
	 (OR #:G927 (PROGN (+ X 1))))
  @end lisp
  "
  (let ((ret (gensym)))
    `(let ((,ret ,form))
       (or ,ret
	   (progn
	     ,@else-body)))))

(defmacro definline (name &body rest)
  "
  Creates a function and declaims them inline: short form for defining an inlined function.

  Example:
  @lisp
  > (macroexpand-1 `(definline f (a b) (+ a b)))
  => (INLINING (DEFUN F (A B) (+ A B)))
  "
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@rest)))

;;---------------------------------------------------------------;;
;; Optimization
;;---------------------------------------------------------------;;
(defmacro with-optimization ((&rest args) &body body)
  "
  Macro creates a local environment with optimization declarations, and
  executes form.

  Example:
  @lisp
  > (macroexpand-1
      `(with-optimization (:speed 2 :safety 3)
	  (+ 1d0 2d0)))
  => (LOCALLY (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 3))) (+ 1.0d0 2.0d0))
  @end lisp
  "
  (destructuring-bind (decl+ body) (trivia:match body
				     ((list* (list* 'declare decl) body) (list decl body))
				     (_ (list nil body)))
    `(locally
       (declare (optimize ,@(trivia:ematch args
			     ((λlist &key speed safety space debug)
			      (remove nil (mapcar #'(lambda (name val) (if val `(,name ,val)))
						  `(speed safety space debug) (list speed safety space debug))))))
		,@decl+)
       ,@body)))

(defmacro very-quickly (&body forms)
  "
  Macro which encloses @arg{forms} inside
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "
  #+matlisp-debug
  `(with-optimization (:safety 3)
     ,@forms)
  #-matlisp-debug
  `(with-optimization (:safety 0 :space 0 :speed 3)
     #+lispworks
     (declare (optimize (harlequin-common-lisp:fixnum-safety 0) (float 0)))
     (let ((matlisp-ffi::*fvref-range-check* nil))
       (locally ,@forms))))

(defmacro eval-every (&body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@forms))


(defmacro with-memoization ((&optional (hash-table `(make-hash-table :test 'equal))) &body body &aux cache need-hashtablep)
  (with-gensyms (table value exists-p args)
    (labels ((transformer (x)
	       (ematch x
		 ((or (list* 'with-memoization _) (list* 'quote _)) x)
		 ((list* 'memoizing body)
		  (match body
		    ((list (λlist 'cl:let bindings &body (or (list* (and (list* 'cl:declare _) decl-p) body) body)
				  &aux (declares (if decl-p (list decl-p))) (id (gensym "memo-"))))
		     (setf need-hashtablep t)
		     `(let (,@bindings)
			,@declares
			(letv* ((,args (list ',id ,@(mapcar #'car bindings)))
				(,value ,exists-p (gethash ,args ,table)))
			  (values-list
			   (if ,exists-p ,value
			       (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))
		    ((list (λlist (and def (or 'cl:defun 'cl:defmethod)) name func-args &body (or (list* (and (list* 'cl:declare _) decl-p) body) body)
				  &aux (declares (if decl-p (list decl-p))) (id (gensym "memo-"))))
		     (setf need-hashtablep t)
		     (assert (not (intersection '(&rest &allow-other-keys) func-args)) nil "can't memoize functions with &rest, &allow-other-keys in their defining lambda-lists")
		     `(,def ,name (,@func-args)
			,@declares
			(letv* ((,args (list ',id ,@(mapcar #'(lambda (x) (first (ensure-list x))) (set-difference func-args cl:lambda-list-keywords))))
				(,value ,exists-p (gethash ,args ,table)))
			  (values-list
			   (if ,exists-p ,value
			       (setf (gethash ,args ,table) (multiple-value-list (progn ,@body))))))))
		    ((list (λlist (and def (or 'cl:labels 'cl:flet)) definitions &body body))
		     (setf need-hashtablep t)
		     `(,def (,@(mapcar #'(lambda (x) (cdr (transformer `(memoizing (cl:defun ,@x))))) definitions))
			  ,@body))
		    ((λlist code &key (type nil type?) (bind (gensym)))
		     (if-let ((cv (rassoc code cache :key #'first :test #'equal)))
		       (first cv)
		       (values (list* bind code (if type? `(:type ,type)))
			       #'(lambda (f decl)
				   (push (list* (first decl) (funcall f (second decl)) (cddr decl)) cache)
				   (first decl))))
		     #+nil(error "don't know how to memoize ~a" code)))))))
      (let ((transformed-body (maptree '(memoizing with-memoization quote) #'transformer body)))
	`(let*-typed (,@(if need-hashtablep `((,table ,hash-table)))
			,@(reverse cache))
	   ,@transformed-body)))))

(defmacro pushcar (x place)
  (with-gensyms (xx)
    `(let ((,xx ,x)) (push ,xx ,place) ,xx)))

(defmacro mapcase ((keyform function) &body cases)
  (with-gensyms (key)
    `(let ((,key ,keyform))
       (cond ,@(mapcar #'(lambda (x)
			   (if (null x) `(t (error "case failure"))
			       `((,function ,key ',(car x)) ,@(cdr x)))) cases)))))

(defmacro recurse-maadi (x match &body dispatchers)
  ;;recurse-ಮಾಡಿ ಸಕ್ಕತ್ತಾಗಿ!
  (assert (eql (first match) :match) nil "invalid dispatch name")
  (let ((macros (mapcar #'(lambda (x) (list* (the (and keyword (not (member :and :or :* :not :.))) (car x))
					     (gensym "dispatch") (cdr x))) (list* match dispatchers))))
    (labels ((recurse (p)
	       (cond
		 ((and (listp p) (member (car p) (list* :and :or :* :not :. (mapcar #'car (cdr macros)))))
		  (case (car p)
		    (:and `(and ,@(mapcar #'recurse (cdr p))))
		    (:or `(or ,@(mapcar #'recurse (cdr p))))
		    ((:* :not) (destructuring-bind (term clause) p
				 `(not ,(if (eql term :*)
					    `(do () ((not ,(recurse clause))))
					    (recurse clause)))))
		    (:. `(locally ,@(cdr p)))
		    (t `(,(second (assoc (car p) macros)) ,@(cdr p)))))
		 (t `(,(second (assoc :match macros)) ,p)))))
      `(macrolet (,@(mapcar #'cdr macros)) ,(recurse x)))))

(defmacro rec (name args &body body)
  (let ((keypos (or (position-if #'(lambda (x) (member x cl:lambda-list-keywords)) args) (length args))))
    `(labels ((,name (,@(mapcar #'first (subseq args 0 keypos)) ,@(subseq args keypos)) ,@body))
       (,name ,@(mapcar #'second (subseq args 0 keypos))))))

(defmacro gethash! (key table default)
  (using-gensyms (decl (key table) (value exists-p))
    `(let (,@decl)
       (letv* ((,value ,exists-p (gethash ,key ,table)))
	 (if ,exists-p (values ,value t) (setf (gethash ,key ,table) ,default))))))

(defmacro values-append (&rest values)
  `(values-list (append ,@(mapcar #'(lambda (x) `(multiple-value-list ,x)) values))))

)
