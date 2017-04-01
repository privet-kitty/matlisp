(in-package #:matlisp-infix)
(pushnew :matlisp-infix *features*)

(defparameter *linfix-reader* (copy-readtable))
(defparameter *blank-characters* '(#\Space #\Tab #\Newline))
(defparameter *exponent-tokens* '(#\E #\S #\D #\F #\L))
(defparameter *operator-tokens*
  `(("⊗" ⊗) ;;<- CIRCLE TIMES
    ("**" **) ("->" ->)
    ("./" ./) ("/" /)
    ("*" *) (".*" .*) 
    ("@" @) ("·" @) ;; <- MIDDLE DOT
    (".+" +) ("+" +)
    (".-" -) ("-" -)
    ("(" \() (")" \))
    ("[" \[) ("]" \])
    (":" |:|) (":=" :=)
    ("←" <-) ("<-" <-)
    ("=" =) (".=" .=)
    ("," \,) ("." \.)
    ("'" ctranspose) (".'" transpose)))

(defun find-token (str stream)
  (let ((stack nil))
    (iter (for r.i in-vector str)
	  (for m.i next (read-char stream t nil t))
	  (push m.i stack)
	  (when (char/= r.i m.i)
	      (map nil #'(lambda (x) (unread-char x stream)) stack)
	      (return nil))
	  (finally (return t)))))

(defun token-reader (stream &optional (enclosing-chars '(#\( . #\))) &aux stack expr lspe)
  (labels ((read-stack (&optional (empty? t))
	     (let* ((fstack (reverse (remove-if #'(lambda (x) (member x *blank-characters*)) stack)))
		    (tok (and fstack (read-from-string (coerce fstack 'string)))))
	       (prog1 tok
		 (when empty?
		   (when fstack (push tok expr))
		   (setf stack nil))))))
    (iter (for c next (peek-char nil stream t nil t))
	  (summing (cond ((char= c (cdr enclosing-chars)) -1) ((char= c (car enclosing-chars)) +1) (t 0)) into count)
	  (cond
	    ((and (char= c (cdr enclosing-chars)) (= count -1))
	     (read-char stream t nil t)
	     (read-stack)
	     (return (values (reverse expr) lspe)))
	    ;;
	    ((member c '(#\# #\\ #\"))
	     (when (char= c #\\) (read-char stream t nil t))
	     (let ((word (read stream))
		   (sym (gensym)))
	       (push sym expr)
	       (push (list sym word) lspe)))
	    ;;
	    ((and (member (char-upcase c) *exponent-tokens*) (numberp (read-stack nil)))
	     (push (read-char stream t nil t) stack)
	     (when (char= (peek-char nil stream t nil t) #\-)
	       (push (read-char stream t nil t) stack)
	       (unless (find (peek-char nil stream t nil t) "0123456789")
		 (unread-char (pop stack) stream))))
	    ((and (char= c #\i) (numberp (read-stack nil)))
	     (read-char stream t nil t)
	     (push (complex 0 (read-stack nil)) expr)
	     (setf stack nil))
	    ;;
	    ((when-let ((tok (find-if #'(lambda (x) (find-token (first x) stream)) (sort (remove-if-not #'(lambda (x) (char= c (aref (first x) 0))) *operator-tokens*) #'> :key #'(lambda (x) (length (first x)))))))
	       (if (and (eql (second tok) '|.|) (integerp (read-stack nil)))
		   (push #\. stack)
		   (progn
		     (read-stack)
		     (push (second tok) expr)))))
	    ((member c *blank-characters*)
	     (read-char stream t nil t)
	     (read-stack))
	    (t (push (read-char stream t nil t) stack))))))

(defun list-lexer (list)
  #'(lambda () (if (null list) (values nil nil)
		   (let* ((value (pop list)))
		     (values (cond ((member value *operator-tokens* :key #'second) value)
				   ((numberp value) 'number)
				   ((symbolp value) 'id)
				   (t (error "Unexpected value ~S" value)))
			     value)))))

(defun funcify (lst)
  (if (symbolp (car lst)) lst
      `(funcall ,(car lst) ,@(cdr lst))))
;;
(yacc:define-parser *linfix-parser*
  (:start-symbol expr)
  (:terminals (-> ** ./ / * .* @ ⊗ + - := <- = .= |(| |)| [ ] |:| |.| |,| ctranspose transpose id number))
  (:precedence ((:left |.| ctranspose transpose)
		(:right **)
		(:left ./ / * .* @ ⊗)
		(:left + -)
		(:left ->)
		(:right := <- = .=)))
  (expr
   (expr ctranspose #'(lambda (a b) (list b a)))
   (expr transpose #'(lambda (a b) (list b a)))
   (expr + expr #'(lambda (a b c) (list b a c)))
   (expr - expr #'(lambda (a b c) (list b a c)))
   (- expr)
   (expr / expr #'(lambda (a b c) (list b a c)))
   (expr ./ expr #'(lambda (a b c) (list b a c)))
   (expr * expr #'(lambda (a b c) (list b a c)))
   (expr .* expr #'(lambda (a b c) (list b a c)))
   (expr @ expr #'(lambda (a b c) (list b a c)))
   (expr ⊗ expr #'(lambda (a b c) (list b a c)))
   (expr ** expr #'(lambda (a b c) (list b a c)))
   (list -> expr #'(lambda (a b c) (declare (ignore b)) `(lambda (,@(cdr a)) ,c)))
   (expr <- expr #'(lambda (a b c) (declare (ignore b)) (list 'setf a c)))
   (expr := expr #'(lambda (a b c) (declare (ignore b)) (list :deflet a c)))
   (expr = expr #'(lambda (a b c) (list b a c)))
   (expr .= expr #'(lambda (a b c) (list b a c)))
   term)
  ;;
  (lid
   id
   (lid |.| id #'(lambda (a b c) (declare (ignore b) (type (not number) a) (type symbol c)) `(slot-value ,a ',c)))
   (lid |.| |.| id #'(lambda (a b c d &aux (aa (gensym "A")))
		       (declare (ignore b c) (type (not number) a) (type symbol d))
		       `(let ((,aa ,a))
			  (slot-value ,aa (find-symbol ,(symbol-name d) (symbol-package (type-of ,aa)))))))
   (|(| expr |)| #'(lambda (a b c) (declare (ignore a c)) b)))
  ;;
  (args
   (expr #'list)
   (expr |,| args #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  ;;
  #+nil
  (1+args
   (expr |,| expr #'(lambda (a b c) (declare (ignore b)) (list a c)))
   (expr |,| 1+args #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  (list
   ([ args ] #'(lambda (a b c) (declare (ignore a c)) (list* 'list b)))
   #+nil
   (|(| 1+args |)| #'(lambda (a b c) (declare (ignore a c)) (list* 'list b))))
  ;;
  (callable
   (lid |(| |)| #'(lambda (a b c) (declare (ignore b c)) (funcify (list a))))
   (lid |(| args |)| #'(lambda (a b c d) (declare (ignore b d)) (funcify (list* a c))))
   (callable |(| |)| #'(lambda (a b c) (declare (ignore b c)) (funcify (list a))))
   (callable |(| args |)| #'(lambda (a b c d) (declare (ignore b d)) (funcify (list* a c)))))
  ;;
  (idxs
   expr
   (|:| #'(lambda (a) (declare (ignore a)) (list :slice nil nil nil)))
   (|:| expr  #'(lambda (a b) (declare (ignore a)) (list :slice nil b nil)))
   (|:| expr |:| expr #'(lambda (a b c d) (declare (ignore a c)) (list :slice nil d b)))
   (expr |:| #'(lambda (a b) (declare (ignore b)) (list :slice a nil nil)))
   (|:| expr |:|  #'(lambda (a b c) (declare (ignore a c)) (list :slice nil nil b)))
   (expr |:| expr #'(lambda (a b c) (declare (ignore b)) (list :slice a c nil)))
   (expr |:| expr |:|  #'(lambda (a b c d) (declare (ignore b d)) (list :slice a nil c)))
   (expr |:| expr |:| expr #'(lambda (a b c d e) (declare (ignore b d)) (list :slice a e c))))
  (sargs
   (idxs #'list)
   (idxs |,| sargs #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  ;;
  (slice
   (callable [ ] #'(lambda (a b c) (declare (ignore b c)) (list 'matlisp-infix::generic-ref a)))
   (callable [ sargs ] #'(lambda (a b c d) (declare (ignore b d)) (list* 'matlisp-infix::generic-ref a c)))
   (lid [ ] #'(lambda (a b c) (declare (ignore b c)) (list 'matlisp-infix::generic-ref a)))
   (lid [ sargs ] #'(lambda (a b c d) (declare (ignore b d)) (list* 'matlisp-infix::generic-ref a c)))
   (slice [ ] #'(lambda (a b c) (declare (ignore b c)) (list 'matlisp-infix::generic-ref a)))
   (slice [ sargs ] #'(lambda (a b c d) (declare (ignore b d)) (list* 'matlisp-infix::generic-ref a c))))
  ;;
  (term
   number lid
   (ctranspose id #'(lambda (a b) (declare (ignore a)) (list 'quote b)))
   (ctranspose |:| id #'(lambda (a b c) (declare (ignore a b)) (intern (symbol-name c) :keyword)))
   list callable slice
   ;;(- term)
   (/ term #'(lambda (a b) (list a b)))
   (./ term)))
;;
(defun process-slice (args)
  (mapcar #'(lambda (x) (if (and (consp x) (eql (car x) :slice)) `(list* ,@(cdr x)) x)) args))

(defmacro generic-ref (x &rest args)
  (cond
    ((null args) x)
    ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
     `(matlisp::subtensor~ ,x (list ,@(process-slice args))))
    (t `(matlisp::ref ,x ,@args))))

(define-setf-expander generic-ref (x &rest args &environment env)
  (multiple-value-bind (dummies vals newval setter getter) (get-setf-expansion x env)
    (declare (ignore setter))
    (with-gensyms (store)
      (values (append dummies newval)
	      (append vals (list getter))
	      `(,store)
	      (let ((arr (car newval)))
		(cond
		  ((null args)
		   `(matlisp::copy! ,store ,arr))
		  ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
		   `(setf (matlisp::subtensor~ ,arr (list ,@(process-slice args))) ,store))
		  (t `(setf (matlisp::ref ,arr ,@args) ,store))))
	      `(generic-ref ,(car newval) ,@args)))))

(defmacro generic-incf (x expr &optional (alpha 1) &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion x env)
    (when (cdr new) (error "Can't expand this."))
    (with-gensyms (val)
      (let ((new (car new)))
	`(let* (,@(zip dummies vals)
		(,new ,getter)
		(,val ,expr))
	   (etypecase ,new
	     (matlisp::base-tensor (matlisp::axpy! ,alpha ,val ,new))
	     (t (setq ,new (+ ,new ,val))))
	   ,setter)))))
;;
(defparameter *operator-assoc-table*
  '((* t:*) (.* t:.*)
    (@ t:@) (⊗ t:⊗) (** t:expt)
    (+ t:+) (- t:-)
    (\\ t::b\\) (/ t:/) (./ t:./)
    (= t:=) (.= t:=)     ;;Not yet implemented.
    (transpose t:transpose) (ctranspose t:ctranspose)))

;; ("sum" t:sum) ("xlogx" t:xlogx) ("realpart" t:realpart) ("imagpart" t:imagpart)
;; ("sqrt" t:sqrt) ("sin" t:sin) ("cos" t:cos) ("tan" t:tan) ("asin" t:asin) ("acos" t:acos) ("atan" t:atan)
;; ("exp" t:exp) ("log" t:log) ("expt" t:expt)
;; ("sinh" t:sinh) ("cosh" t:cosh) ("tanh" t:tanh) ("asinh" t:asinh) ("acosh" t:acosh) ("atanh" t:atanh)


(defun op-overload (expr &aux (table *operator-assoc-table*))
  (maptree-eki #'(lambda (tree)
		   (trivia:match tree
		     ((list (or '+ '* 'progn) arg) (values arg #'(lambda (f tree) (funcall f tree))))
		     ((list* func args) (values `(,(or (second (assoc func table)) func) ,@args) t))
		     (_ (values tree t))))
	       expr))

(defun ignore-characters (ignore stream)
  (iter (for c next (peek-char nil stream t nil t))
	(if (member c ignore :test #'char=) (read-char stream t nil t) (terminate))))
;;
(defmacro inlet (&rest body &aux decls)
  (let ((code (maptree-eki #'(lambda (mrk)
			       (trivia:ematch mrk
				 ((list :deflet arg value) (push arg decls)
				  (values `(setf ,arg ,value) t))
				 ((list :deflet (list* 'list args) value)
				  (setf decls (append (reverse args) decls))
				  (values `(setf (values ,@args) ,value) t))
				 (_ (values mrk t))))
			   body)))
    (if (or decls (cdr code))
	`(let (,@decls) ,@code)
	(car code))))

(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  (ignore-characters *blank-characters* stream)
  (letv* ((iexpr bind (token-reader stream (ecase (read-char stream t nil t) (#\( (cons #\( #\))) (#\[ (cons #\[ #\])))))
	  (lexpr (op-overload (yacc:parse-with-lexer (list-lexer (nconc (list 'inlet '\() iexpr (list '\)))) *linfix-parser*))))
    (map nil #'(lambda (x) (setf lexpr (subst (second x) (first x) lexpr))) bind)
    lexpr))
;;
(eval-every
  (defparameter *tensor-symbol*
    `((#\D ,(matlisp:tensor 'cl:double-float))
      (#\E ,(matlisp:tensor 'cl:single-float))
      (#\Z ,(matlisp:tensor '(cl:complex cl:double-float)))
      (#\Q ,(matlisp:tensor 'cl:rational))
      (#\B ,(matlisp:tensor 'cl:bit)))))

(defun tensor-reader (stream subchar arg)
  (assert (null arg) nil "given arg where none was required.")
  (let ((cl (second (find subchar *tensor-symbol* :key #'car))))
    (ignore-characters *blank-characters* stream)
    (ecase (peek-char nil stream t nil t)
      (#\[ (let ((expr (cdr (infix-reader stream #\I nil))))
	     `(matlisp::copy (list ,@expr) ',cl)))
      (#\( (let ((expr (cdr (infix-reader stream #\I nil))))
	     `(matlisp::zeros (list ,@expr) ',cl))))))

#+weyl
(defun symbolic-reader (stream subchar arg)
  (assert (null arg) nil "given arg where none was required.")
  (let ((cl (matlisp::tensor 'matlisp::ge-expression)))
    (ignore-characters *blank-characters* stream)
    (let ((sym (unless (member (peek-char nil stream t nil t) '(#\[ #\())
		 (intern (iter (for c next (read-char stream t nil t))
			       (cond
				 ((char= c #\[) (error "can't symbolify a specified tensor, use parentheses."))
				 ((or (char= c #\() (member c *blank-characters*)) (unread-char c stream) (return (coerce sname 'string)))
				 ((> n 32) (error "can't exceed 32 characters for var name.")))
			       (counting t into n)
			       (collect (char-upcase c) into sname))))))
      (ignore-characters *blank-characters* stream)
      (ecase (peek-char nil stream t nil t)
	(#\[ (let ((expr (cdr (infix-reader stream #\I nil))))
	       `(matlisp::copy (list ,@(mapcar #'matlisp::weylify expr)) ',cl)))
	(#\( (let ((expr (cdr (infix-reader stream #\I nil))))
	       (recursive-append
		(when sym `(matlisp::symbolify! ',sym))
		`(matlisp::zeros (list ,@expr) ',cl))))))))

;(#\S ,(matlisp:tensor 'matlisp::ge-expression))
(defun permutation-cycle-reader (stream subchar arg)
  (declare (ignore subchar))
  (assert (null arg) nil "given arg where none was required.")
  (ignore-characters *blank-characters* stream)
  (ecase (peek-char nil stream t nil t)
    (#\[ (let ((expr (cdr (infix-reader stream #\I nil))))
	   (with-gensyms (sto)
	     `(let ((,sto (mapcar #'(lambda (x) (apply #'matlisp::idxv x)) (list ,@expr))))
		(make-instance 'matlisp::permutation-cycle :store ,sto )))))))

(defun memoization-reader (stream subchar char)
  (declare (ignore subchar))
  `(memoizing ,@(read stream t nil t) :global ,(if char t)))

;;Define a readtable with dispatch characters
(macrolet ((tensor-symbol-enumerate ()
	     `(named-readtables:defreadtable :infix-dispatch-table
		(:merge :λ-standard)
		(:dispatch-macro-char #\# #\I #'infix-reader)
		(:dispatch-macro-char #\# #\S #'permutation-cycle-reader)
		,@(mapcar #'(lambda (x) `(:dispatch-macro-char #\# ,(car x) #'tensor-reader)) *tensor-symbol*)
		#+weyl
		(:dispatch-macro-char #\# #\स #'symbolic-reader)
		(:dispatch-macro-char #\# #\m #'memoization-reader))))
  (tensor-symbol-enumerate))
