(in-package #:matlisp-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;Note to self: do not indent!

(defmacro define-constant (name value &optional doc)
  "
  Keeps the lisp implementation from defining constants twice.
  "
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro with-gensyms (symlist &body body)
  "
  Binds every variable in @arg{symlist} to a (gensym).

  Example:
  @lisp
  > (macroexpand-1
       `(with-gensyms (a b c)
	   `(let ((,a 1) (,b 2) (,c 3))
		 (+ ,a ,b ,c))))
  => (LET ((A (GENSYM \"A\")) (B (GENSYM \"B\")) (C (GENSYM \"C\")))
      `(LET ((,A 1) (,B 2) (,C 3))
	  (+ ,A ,B ,C)))
  @end lisp
  "
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym ,(symbol-name sym))))
		 symlist)
     ,@body))

(defmacro using-gensyms ((decl (&rest syms) &optional gensyms) &rest body)
  `(let ((,decl (zip ',(mapcar #'(lambda (x) (gensym (symbol-name x))) syms) (list ,@syms))))
     (destructuring-bind (,@syms) (mapcar #'car ,decl)
       ,(append
	 (if gensyms
	   `(with-gensyms (,@gensyms)) `(progn))
	 body))))

(defmacro binding-gensyms ((mname &optional (fname (gensym))) &rest body)
  (with-gensyms (htbl)
    `(let ((,htbl (make-hash-table)))
       (labels ((,fname (x) (or (gethash x ,htbl) (setf (gethash x ,htbl) (gensym (symbol-name x))))))
	 (macrolet ((,mname (x) `(,', fname ',x)))
	   ,@body)))))

(defmacro ziprm ((r m) &rest args)
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
       (cond ,@(mapcar #'(lambda (clause) `((and ,@(mapcar #'(lambda (x) (list* 'eql x)) (remove-if #'(lambda (x) (eql (cadr x) t)) (zip (mapcar #'car decl) (first clause))))) ,@(cdr clause))) cases)))))

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
	       `((let ((,(first (car rets)) ,(maptree '(values-n previous-value) #'(lambda (x) (case (car x)
												 (values-n x)
												 (previous-value
												  (destructuring-bind (&optional (idx (- i 2))) (cdr x)
												    (assert (< -1 idx (length vrets)) nil 'invalid-arguments)
												    (elt (reverse vrets) idx)))))
						      (second (car rets)))))
		   ,(recursive-append
		     (when (cdr rets)
		       `(if (> ,n ,i) ,@(make-cd (1+ i) (cdr rets) (cons (caar rets) vrets))))
		     `(values ,@(reverse vrets) ,(caar rets)))))))
      `(let (,@decl)
	 (when (> ,n 0)
	   ,@(make-cd 1 (zipsym values) nil))))))

(defmacro letv* (bindings &rest body)
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
			    (let ((flt (remove consy (flatten bind))))
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

(defmacro let-typed (bindings &rest body)
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

(defmacro let*-typed (bindings &rest body)
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

(defmacro if-ret (form &rest else-body)
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

(defmacro when-let ((var . form) &rest body)
  "
  Binds the result of @arg{form} to the symbol @arg{var}; if this value
  is non-nil, the s-expression @arg{body} is executed.

  Example:
  @lisp
  > (macroexpand-1
      `(when-let (parity (evenp x))
	     (+ x 1)))
  => (LET ((PARITY (EVENP X)))
	(WHEN PARITY (+ X 1)))
  @end lisp
  "
  (check-type var symbol)
  `(let ((,var ,@form))
     (when ,var
       ,@body)))

(defmacro if-let ((var . form) &rest body)
  "
  Binds the result of @arg{form} to the symbol @arg{var}; this value
  is used immediately in an if-statement with the usual semantics.

  Example:
  @lisp
  > (macroexpand-1
      `(if-let (parity (evenp x))
	     (+ x 1)
	     x))
  => (LET ((PARITY (EVENP X)))
	(IF PARITY
	   (+ X 1)
	   X))
  @end lisp
  "
  (check-type var symbol)
  `(let ((,var ,@form))
     (if ,var
	 ,@body)))

(defmacro definline (name &rest rest)
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
(defmacro with-optimization ((&rest args) &body forms)
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
  `(locally
       ,(recursive-append
	 `(declare (optimize ,@(multiple-value-call #'mapcar #'(lambda (key val) (list (intern (symbol-name key)) val))
						    (loop :for ele :in args
						       :counting t :into cnt
						       :if (oddp cnt)
							 :collect ele into key
						       :else
							 :collect (progn (assert (member ele '(0 1 2 3))) ele) into val
						       :finally (return (values key val))))))
	 (when (and (consp (car forms)) (eq (caar forms) 'declare))
	   (cdar forms)))
     ,@(if (and (consp (car forms)) (eq (caar forms) 'declare)) (cdr forms) forms)))

(defmacro very-quickly (&body forms)
  "
  Macro which encloses @arg{forms} inside
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "
  #+matlisp-debug
  `(with-optimization
       #+lispworks
       (:safety 3)
       #-lispworks
       (:safety 3)
     ,@forms)
  #-matlisp-debug
  `(with-optimization
       #+lispworks
       (:safety 0 :space 0 :speed 3 :float 0 :fixnum-safety 0)
       #-lispworks
       (:safety 0 :space 0 :speed 3)
     ,@forms))

(defmacro eval-every (&body forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@forms))

(defmacro with-memoization ((&optional (hash-table `(make-hash-table :test 'equal))) &rest body)
  (with-gensyms (table value exists-p args)
    (maptree '(memoizing-let defmem with-memoization)
	     #'(lambda (x)
		 (if (eq (first x) 'with-memoization) x
		     (let* ((id (gensym "memo-"))
			    (def-p (eql (first x) 'defmem))
			    (decl-p (let ((lst (nth (+ 2 (if def-p 1 0)) x)))
				      (and (consp lst) (eql (car lst) 'declare)))))
		       `(,(if def-p 'defun 'let) ,@(subseq x 1 (+ (if def-p 1 0) (if decl-p 3 2)))
			  (letv* ((,args (list ',id ,@(if def-p
							  (mapcar #'(lambda (x) (if (consp x) (car x) x)) (remove-if #'(lambda (x) (member x cl:lambda-list-keywords)) (third x)))
							  (mapcar #'car (second x)))))
				  (,value ,exists-p (gethash ,args ,table)))
			    (values-list
			     (if ,exists-p ,value
				 (setf (gethash ,args ,table) (multiple-value-list (progn ,@(nthcdr (+ (if def-p 1 0) (if decl-p 3 2)) x)))))))))))
	     `(let ((,table ,hash-table)) ,@body))))

(defmacro curry (func &rest more-funcs)
  (with-gensyms (x)
    `(lambda (,x) ,(reduce #'(lambda (f a)
			       (let ((f (trivia:match f
					  ((list 'function x) x)
					  (_ f))))
				 `(,f ,a)))
			   (append `(,func) more-funcs `(,x)) :from-end t))))

(defmacro pushcar (x place)
  (with-gensyms (xx)
    `(let ((,xx ,x)) (push ,xx ,place) ,xx)))

(defmacro mapcase ((keyform function) &body cases)
  (with-gensyms (key)
    `(let ((,key ,keyform))
       (cond ,@(mapcar #'(lambda (x)
			   (if (null x) `(t (error "case failure"))
			       `((,function ,key ',(car x)) ,@(cdr x)))) cases)))))

(defmacro recurse-maadi (x match &rest dispatchers)
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

(defmacro rec (name args &rest body)
  (let ((keypos (or (position-if #'(lambda (x) (member x cl:lambda-list-keywords)) args) (length args))))
    `(labels ((,name (,@(mapcar #'first (subseq args 0 keypos)) ,@(subseq args keypos)) ,@body))
       (,name ,@(mapcar #'second (subseq args 0 keypos))))))
)
