(in-package #:matlisp-user)

(defmacro lift-function (fn &aux (pkg (find-package "MATLISP-USER")))
  (letv* ((fname (symbol-name fn)) (fpkg (symbol-package fn)))
    (shadow fn pkg)
    (letv* ((fn (find-symbol fname fpkg))
	    (fn-package (intern fname pkg))
	    (ge-fn (intern (string+ fname "-GENERIC!") pkg)))
      `(progn
	 (eval-every
	   (let ((package (find-package "MATLISP-USER")))
	     (shadow ',fn-package package)
	     (export ',fn-package package)))
	 (closer-mop:defgeneric ,ge-fn (x)
	   (:generic-function-class tensor-method-generator))
	 (define-tensor-method ,ge-fn ((x dense-tensor :x))
	   `(dorefs (idx (dimensions x))
		    ((ref-x x :type ,(matlisp::cl :x)))
		    (setf ref-x (,',fn ref-x)))
	   'x)       
	 (definline ,(intern (string+ fname "!") (find-package "MATLISP-USER")) (x)
	   (etypecase x
	     (number (,fn x))
	     (tensor (,ge-fn x))))
	 (definline ,fn-package (x)
	   (etypecase x
	     (number (,fn x))
	     (tensor (,ge-fn (copy x)))))))))

(macrolet ((lift-fns (&rest lst)
	     `(progn ,@ (mapcar #'(lambda (x) `(lift-function ,x)) lst))))
  (lift-fns cl:sin cl:cos cl:tan cl:asin cl:acos cl:exp))
