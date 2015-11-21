(in-package #:matlisp-destructuring)

#+nil
(defun parse-integerm (&aux d)
  (recurse-maadi (:and (:or #\+ #\- (:and)) (:digit? d) (:* (:digit? d)))
    (:match (x) `(when (eql (peek-char) ',x) (read-char)))
    (:digit? (v) `(when (char<= #\0 (peek-char) #\9) (setq ,v (read-char))))))

;;Add an exit for the state machine.
(define-condition destruct-compiler-error (error) ())
(deftype λlist-keyword () `(member ,@lambda-list-keywords))
(deftype variable-symbol () `(and symbol (not λlist-keyword) (not keyword) (not boolean)))
(defun type-check (type place) `(assert (typep ,place ',type) nil 'destruct-compiler-error))
(defmacro getf! (place indicator &optional default &environment env)
  (letv* ((dummies vals new setter getter (get-setf-expansion place env)))
    (if (cdr new) (error "Can't expand this.")
	(setq new (car new)))
    (with-gensyms (x x- lst+ ind)
      `(let* (,@(zip dummies vals)
	      (,lst+ (cons :head ,getter))
	      (,ind ,indicator))
	 (do ((,x (cdr ,lst+) (cddr ,x))
	      (,x- ,lst+ (cdr ,x)))
	     ((or (not ,x)
		  (progn (type-check cons (cdr ,x)) (type-check keyword (car ,x))
		    (eql (first ,x) ,ind)))
	      (if (not ,x) ,default
		  (progn (setf (cdr ,x-) (cddr ,x))
		    (let ((,new (cdr ,lst+))) ,setter)
		    (second ,x)))))))))
(defmacro plist-check (lst)
  (with-gensyms (ll x)
    `(let ((,ll ,lst))
       (do ((,x ,ll (cddr ,x)))
	   ((or (not ,x) (progn (type-check cons (cdr ,x)) (type-check keyword (car ,x)) nil)) t)))))

(defun destruct-parser (pattern &optional sub-destruct? dotted? &aux (ptn pattern) (seq (gensym "sequence")) (end (gensym "end")) compiler)
  (labels ((parse-rest (p &optional nullify?)
	     (push `(,(the variable-symbol p) ,@(if nullify? `((prog1 ,seq (setf ,seq nil))) seq)) compiler))
	   (parse-atom (p)
	     (typecase p
	       (variable-symbol (push `(,(the variable-symbol p) (progn (type-check cons ,seq) (pop ,seq))) compiler))
	       (cons (if sub-destruct?
			 (letv* ((p-seq cr (destruct-parser p t dotted?)))
			   (push `(,p-seq (progn (type-check cons ,seq) (pop ,seq))) compiler)
			   (setf compiler (append (reverse cr) compiler)) t)
			 (error "given sub-destruct sequence")))))
	   (parse-optional (p)
	     (when (typep p '(or cons variable-symbol))
	       (letv* (((var &optional (default nil) key) (ensure-list p) :type (variable-symbol &optional (t) (or variable-symbol t))))
		 (if key (push `(,key nil) compiler))
		 (push `(,var (if (not ,seq) ,default
				 (progn (type-check cons ,seq)
				   ,@(if key `((setf ,key t)))
				   (pop ,seq))))
		       compiler))))
	   (parse-key (p)
	     (when (typep p '(or cons variable-symbol))
	       (letv* (((var &optional (default nil) key) (ensure-list p) :type (variable-symbol &optional (t) (or variable-symbol t))))
		 (if key (push `(,key nil) compiler))
		 (push `(,var (getf! ,seq ,(intern (symbol-name var) :keyword) 
				     (progn ,@(if key `((setf ,key t))) ,default)))
		       compiler))))
	   (parse-aux (p)
	     (when p
	       (letv* (((var &optional (expr nil)) (ensure-list p) :type (variable-symbol &optional (t))))
		 (push (list var expr) compiler)))))
    (if (recurse-maadi
	 (:and
	  ;;&whole
	  (:or (:and '&whole (:. (parse-rest (second ptn))) (:pop 2))
	       (:and))
	  ;;args
	  (:* (:and (:. (listp ptn)) (:. (parse-atom (first ptn))) (:pop)))
	  ;;&optional
	  (:or (:not (:. (listp ptn)))
	       (:and '&optional (:pop)
		     (:* (:and (:. (parse-optional (first ptn))) (:pop))))
	       (:and))
	  ;;&rest
	  (:or (:and (:. (and dotted? (not (listp ptn)))) (:. (parse-rest ptn t) (setq ptn nil)))
	       (:and (:or '&rest '&body) (:. (parse-rest (second ptn))) (:pop 2))
	       (:and))
	  ;;&key
	  (:or (:and '&key (:pop) (:. (push `(,seq (copy-list ,seq)) compiler))
		     (:* (:and (:. (parse-key (first ptn))) (:pop)))
		     ;;&allow-other-keys
		     (:or (:and '&allow-other-keys (:pop) (:. (push `(,end (plist-check ,seq)) compiler)))
			  (:. (push `(,end (type-check null ,seq)) compiler))))
	       (:and))
	  ;;&aux
	  (:or (:and '&aux (:pop) (:* (:and (:. (parse-aux (first ptn))) (:pop))))
	       (:and))
	  (:. (null ptn)))
	 ;;
	 (:match (x) `(eql (car ptn) ,x))
	 (:pop (&optional (n 1)) `(progn ,@(iter (repeat n) (collect `(pop ptn))))))
	(values seq (reverse compiler)))))

;;
;(destruct-parser '(a b &rest k &key c &allow-other-keys &aux (x 1)))


;; (defmacro my-destructuring-bind (lambda-list expression &body body)
;;   (letv* ((seq-name bind* (destruct-compiler lambda-list t t)))
;;     `(let* ((,seq-name ,expression)
;; 	    ,@bind*)
;;        ,@body)))

;; (macrolet ((type-check (ty x) `(the ,ty ,x)))
;;   (time
;;    (my-destructuring-bind (a &key b) (list 1 :z 2)
;;      (list a b)))
;;   (time
;;    (destructuring-bind (a (b . c)) (list 1 '(2 . 3))
;;      (list a b c)))
;;   )

;; (destructuring-bind (a &allow-other-keys &key c . d) (list 1 :c 2)
;;   )
