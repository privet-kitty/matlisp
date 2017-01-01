(in-package #:matlisp-utilities)

;;These functions are used all over the place inside Matlisp's macros.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun remmeth (func spls &optional quals)
  (let ((meth (find-method func quals (mapcar #'(lambda (x) (if (consp x) x (find-class x))) spls) nil)))
    (when meth
      (remove-method func meth)
      meth)))

(defun maptree-if (predicate transformer tree)
  "
  Returns a new tree by recursively calling @arg{transformer} on sub-trees which satisfy the @arg{predicate}.
  @arg{predicate} : tree -> boolean
  @arg{transformer}: tree -> (or tree atom) *control
  If the transformer returns a @arg{control} function, then the tree returned by
  the transformer is replaced in-turn by the result of:
  > (funcall @arg{control} #'(lambda (x) (maptree-if @arg{predicate} @arg{transformer} x)) transformed-tree)
  , otherwise it is left as it is.

  Example:
  @lisp
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
		#'(λ (x) `(pong ,@(cdr x)))
		'(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PING (PING 1))))
  > (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
		#'(λ (x) (values `(pong ,@(cdr x)) #'mapcar))
		'(progn (ping (ping (ping 1)))))
  >= (PROGN (PONG (PONG (PONG 1))))
  @end lisp
  "
  (multiple-value-bind (t-tree control) (if (funcall predicate tree)
					    (funcall transformer tree)
					    (values tree #'mapcar))
    (if (and (consp t-tree) control)
	(funcall control #'(lambda (x) (maptree-if predicate transformer x)) t-tree)
	t-tree)))

(defun maptree (keys transformer tree)
  (maptree-if (if (eql keys t)
		  #'(lambda (x) (declare (ignore x)) t)
		  #'(lambda (x) (and (consp x) (member (car x) keys))))
	      (if (or (eql keys t) (functionp transformer)) transformer
		  (let ((alist (mapcar #'(lambda (x y) (cons x y)) keys transformer)))
		    #'(lambda (x) (values (cons (cdr (assoc (car x) alist)) (cdr x)) #'mapcar))))  tree))

(defun maptree-eki (transformer tree)
  (multiple-value-bind (t-tree control) (funcall transformer tree)
    (if (and (consp t-tree) control)
	(funcall (if (eql control t) #'mapcar control) #'(lambda (x) (maptree-eki transformer x)) t-tree)
	t-tree)))

(defun cart (list &rest more-lists)
  (if more-lists
      (mapcan #'(lambda (y) (mapcar #'(lambda (x) (cons x y)) list)) (apply #'cart more-lists))
      (mapcar #'list list)))

(defun mapcart (function list &rest more-lists)
  (mapcar (lambda (args) (apply function args)) (apply #'cart list more-lists)))

(declaim (inline pair))
(defun pair (list &optional (n 2))
  (loop :for x :on list :by #'(lambda (x) (nthcdr n x))
     :collect (let ((xth x)) (loop :repeat n :collect (pop xth)))))

(declaim (inline zip))
(defun zip (&rest args)
  "
  Zips the elements of @arg{args}.

  Example:
  @lisp
  > (zip '(2 3 4) '(a b c) '(j h c s))
  => ((2 A J) (3 B H) (4 C C))
  @end lisp
  "
  (apply #'map 'list #'list args))

(defun unzip (list)
  "
  UnZips the elements of @arg{args}.

  Example:
  @lisp
  > (unzip ((2 A J) (3 B H) (4 C C)))
  => ((2 3 4) (a b c) (j h c))
  @end lisp
  "
  (mapcar #'(lambda (n) (mapcar #'(lambda (x) (elt x n)) list))
	  (iter (for i from 0 below (length (first list))) (collect i))))

(defun ziptree (tree &rest more-trees)
  (if (atom tree)
      (cons tree more-trees)
      (apply #'mapcar (list* #'ziptree tree more-trees))))

(declaim (inline zipsym))
(defun zipsym (lst)
  "
  Zips a unique gensym with each element of @arg{lst}.

  Example:
  @lisp
  > (zipsym '(a b c))
  => ((#:G1064 A) (#:G1065 B) (#:G1066 C))
  @end lisp
  "
  (map 'list #'(lambda (x) (list (gensym) x)) lst))
;;
(defun deconsify (x sym)
  (if (atom x) x
      (iter (for ll on x)
	    (collect (deconsify (car ll) sym))
	    (when (and (cdr ll) (not (consp (cdr ll))))
	      (collect sym)
	      (collect (deconsify (cdr ll) x))))))

(defun reconsify (x sym)
  (if (atom x) x
      (iter (for ll on x) (with right = nil)
	    (if (eql (car ll) sym)
		(progn (assert (not (caddr ll)) nil "Misformed x")
		  (setf right (cadr ll)) (finish))
		(collect (reconsify (car ll) sym) into left))
	    (finally (if right (setf (cdr (last left)) right)) (return left)))))
;;
(defun recursive-append (&rest lsts)
  "
  Appends lists in a nested manner, mostly used to bring in the charm of
  non-lispy languages into macros.

  Basically does
  @lisp
  (reduce
    #'(lambda (x y)
	(if (null x)
	  (if (typep (car y) 'symbol) y (car y))
	    (append x (if (null y) nil
			(if (typep (car y) 'symbol) `(,y) y)))))
    lsts :from-end t)
  @end lisp

  Examples:
  @lisp
  > (recursive-append
      '(let ((x 1)))
      '(+ x 2))
  => (LET ((X 1))
       (+ X 2))

  > (recursive-append
      '(let ((x 1)))
      '((let ((y 2))
	  (setq y 3))
	(let ((z 2))
	  z)))
  => (LET ((X 1))
       (LET ((Y 2))
	 (SETQ Y 3))
       (LET ((Z 2))
	 Z))

  > (recursive-append
      nil
      '((let ((x 1)) x)
	(progn (+ 1 2))))
  => (LET ((X 1))
       X)

  > (recursive-append nil '(let ((x 1)) x))
  => (LET ((X 1))
       X)
  @end lisp
  "
  (reduce #'(lambda (x y)
	      (if (null x)
		  (if (typep (car y) 'symbol) y (car y))
		  (append x (and y (if (typep (car y) 'symbol) `(,y) y)))))
	  lsts :from-end t))

(defun compile-and-eval (source)
  "
  Compiles and evaluates the given @arg{source}.  This should be
  an ANSI compatible way of ensuring method compilation."
  (funcall (compile nil `(lambda () ,source))))

;;Helper functions
(declaim (inline modproj))
(defun modproj (i d &optional open? def)
  (cond
    ((not i) def) ((not d) i)
    (t (assert (if open? (<= (- (1+ d)) i d) (< (- (1+ d)) i d)) nil 'tensor-index-out-of-bounds)
       (if (< i 0) (if (and open? (= i (- (1+ d)))) -1 (mod i d)) i))))

(defun infer-type (expr env)
  (or
   (trivial-types:type-expand
    (match expr
      ((list 'the type _) type)
      ((type number) (type-of expr))
      ((list 'quote thing) (type-of thing))
      ((type null) 'null)
      #+(or sbcl)
      ((type symbol)
       (multiple-value-bind (binding-type localp declarations) (#+sbcl sb-cltl2:variable-information
								       expr env)
	 (declare (ignore binding-type localp))
	 (let ((type-decl (find 'type declarations :key #'car)))
	   (and type-decl (cdr type-decl)))))))
   t))

(defun make-extensible-array (&optional (element-type t))
  (make-array 0 :fill-pointer t :adjustable t :element-type element-type))

;; (defstruct (sap-wrap (:constructor make-sap-wrap (ptr)))
;;   (ptr (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))

;; (defun sap-wrap (ptr &optional finalizer)
;;   (let ((wrap (make-sap-wrap ptr)))
;;     (if finalizer
;; 	(tg:finalize wrap
;; 		     (typecase finalizer
;; 		       ((eql t) #'(lambda () (cffi:foreign-free ptr)))
;; 		       (function #'(lambda () (funcall finalizer ptr))))))
;;     wrap))

)
