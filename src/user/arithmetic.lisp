(in-package #:matlisp-user)

;;All the b macros will (only) assume that the first argument is destructible
;;when both arguments are supplied (i.e (not (null b))).

(defmacro b+ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:+ ,a ,b))
	 ((number null) ,a)
	 ((tensor (or tensor number)) (axpy! 1 ,b ,a))
	 ((number tensor) (axpy 1 ,a ,b))
	 ((tensor null) (copy ,a))))))

(definline + (&rest objects &aux (ret (if objects (b+ (car objects)) 0)))
  (iter (for fst in (cdr objects)) (setf ret (b+ ret fst)))
  ret)

(define-compiler-macro + (&rest objects)
  (rec reducer ((ret (if objects `(b+ ,(car objects)) 0)) (objs (cdr objects)))
       (if (not objs) ret
	   (reducer `(b+ ,ret ,(car objs)) (cdr objs)))))

#+nil
(defun test+ ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (+ a a)))
       (number (dotimes (i 100000) (+ a a)))))))
;;
(defmacro b- (a &optional b)  
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:- ,a ,b))
	 ((number null) (cl:- ,a))	 
	 ((tensor (or tensor number)) (axpy! -1 ,b ,a))
	 ((number tensor) (b+ (scal -1 ,b) ,a))
	 ((tensor null) (scal -1 ,a))))))

(definline - (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
	(iter (for fst in more-objects) (setf ret (b- ret fst)))
	ret)
      (b- object)))

(define-compiler-macro - (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
	   (if (not objs) ret
	       (reducer `(b- ,ret ,(car objs)) (cdr objs))))
      `(b- ,object)))

#+nil
(defun test- ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (- a a a)))
       (number (dotimes (i 100000) (- a a a)))))))
;;
(defmacro b.* (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:* ,a ,b))
	 ((number null) ,a)
	 ((tensor (or tensor number)) (scal! ,b ,a))
	 ((number tensor) (scal ,a ,b))
	 ((tensor null) (copy ,a))))))

(definline .* (&rest objects &aux (ret (if objects (b.* (car objects)) 1)))
  (iter (for fst in (cdr objects)) (setf ret (b.* ret fst)))
  ret)

(define-compiler-macro .* (&rest objects)
  (rec reducer ((ret (if objects `(b.* ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret
	   (reducer `(b.* ,ret ,(car objs)) (cdr objs)))))
;;
(defmacro b* (a &optional b)
  (trivia:match* (a b)
    (((or (list (and op-a (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-a) code-a)
      (or (list (and op-b (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-b) code-b))
     (using-gensyms (decl (code-a code-b))
       (symbol-macrolet ((a (if op-a `(,op-a ,code-a) code-a))
			 (b (if op-b `(,op-b ,code-b) code-b)))
	 `(let (,@decl)
	    (cart-etypecase (,code-a ,code-b)
	      ((number number) (cl:* ,a ,b))
	      ((number null) ,a)
	      ;;Scaling
	      ((number tensor) (scal ,a ,b))
	      ((tensor number) (scal ,b ,a))
	      ;;Matrix, vector/matrix product
	      ((tensor-matrix (or tensor-matrix tensor-vector)) (gem 1 ,a ,b nil nil))
	      ((tensor null) (copy ,a))
	      ;;Permutation action. Left action permutes axis-0, right action permutes the last axis (-1).
	      ((permutation base-tensor) (permute ,b ,a 0))
	      ((tensor permutation) (permute ,a ,b -1))
	      ;;The correctness of this depends on the left-right order in reduce (foldl).
	      ((permutation permutation) (permutation* ,a ,b))
	      ((permutation null) (copy ,a)))))))))

(definline * (&rest objects &aux (ret (if objects (b* (car objects)) 1)))
  (iter (for fst in (cdr objects)) (setf ret (b* ret fst)))
  ret)

(define-compiler-macro * (&rest objects)
  (rec reducer ((ret (if objects `(b* ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret	  
	   (trivia:match (car objs)
	     ((list '/ a)
	      (if (cdr objs)
		  `(b* ,ret (b\\ ,@(if (cddr objs) `((* ,@(cdr objs))) (cdr objs)) ,a))
		  `(b/ ,ret ,a)))
	     (_ (reducer `(b* ,ret ,(car objs)) (cdr objs)))))))
;;
(defmacro b./ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:/ ,a ,b))
	 ((number null) (cl:/ ,a))
	 ((tensor (or tensor number)) (div! ,b ,a))
	 ((number tensor) (div ,b ,a))
	 ((tensor null) (div ,a 1))))))

(definline ./ (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
	(iter (for fst in more-objects) (setf ret (b./ ret fst)))
	ret)
      (b./ object)))

(define-compiler-macro ./ (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
	   (if (not objs) ret
	       (reducer `(b./ ,ret ,(car objs)) (cdr objs))))
      `(b./ ,object)))
;;
(defmacro b@ (a &optional b)
  (trivia:match* (a b)
    (((or (list (and op-a (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-a) code-a)
      (or (list (and op-b (or 'ctranspose 'ctranspose~ 'transpose 'transpose~)) code-b) code-b))
     (using-gensyms (decl (code-a code-b))
       (symbol-macrolet ((a (if op-a `(,op-a ,code-a) code-a))
			 (b (if op-b `(,op-b ,code-b) code-b)))
	 `(let (,@decl)
	    (cart-etypecase (,code-a ,code-b)
	      ((number number) (cl:* ,a ,b))
	      ((number null) ,a)
	      ;;Scaling
	      ((number tensor) (scal ,a ,b))
	      ((tensor number) (scal ,b ,a))
	      ;;Matrix, vector/matrix product
	      ((tensor-vector tensor-vector) (dot ,a ,b nil))
	      ((tensor-matrix (or tensor-matrix tensor-vector)) (gem 1 ,a ,b nil nil))
	      ((tensor-vector tensor-matrix) (gem 1 ,b ,a nil nil :t))
	      ((tensor tensor) (gett! 1 ,a ,b 1 (zeros (append (butlast (dimensions ,code-a t)) (cdr (dimensions ,code-b t))) (class-of ,code-a))))
	      ((tensor null) (copy ,a))
	      ;;Permutation action on arguments. Left action unpermutes arguments, right action permutes them.
	      ;;See tb* for comparison.
	      ((permutation tensor) (transpose ,b (permutation/ ,a)))
	      ((tensor permutation) (transpose! ,a ,b))
	      ;;The correctness of this depends on the left-right order in reduce (foldl).
	      ((permutation permutation) (permutation* ,a ,b))
	      ((permutation null) (copy ,a)))))))))

(definline @ (&rest objects &aux (ret (if objects (b@ (car objects)) 1)))
    (iter (for fst in (cdr objects)) (setf ret (b@ ret fst)))
  ret)

(define-compiler-macro @ (&rest objects)
  (rec reducer ((ret (if objects `(b@ ,(car objects)) 1)) (objs (cdr objects)))
       (if (not objs) ret
	   (reducer `(b@ ,ret ,(car objs)) (cdr objs)))))

(definline · (&rest objects) (apply #'@ objects))
#+nil
(defun test· ()
  (let ((a (print (if (cl:= (random 2) 0) 1 (randn 1)))))
    (time (dotimes (i 100000) (· a a)))
    #+nil
    (time
     (etypecase a
       (tensor (dotimes (i 100000) (+ a a)))
       (number (dotimes (i 100000) (+ a a)))))))
;;
(defmacro b/ (a &optional b)
  "Solve x b = a (a /b); or compute /a"
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:/ ,a ,b))
	 ((number null) (cl:/ ,a))
	 ((tensor number) (div! ,b ,a))
	 (((or tensor-vector tensor-matrix) tensor-square-matrix) ;; (/b' a')' =  a / b	  
	  (copy! (transpose!
		  (let ((m:*default-stride-ordering* :col-major))
		    (getrs! (getrf! (copy ,b)) (transpose ,a) :t)))
		 ,a))
	 ((tensor-square-matrix null) (getri! (getrf! (copy ,a))))
	 ((tensor permutation) (permute! ,a (permutation/ ,b) -1))
	 ;;The correctness of this depends on the left-right order in reduce (foldl).
	 ((permutation permutation) (permutation* ,a (permutation/ ,b)))
	 ((permutation null) (permutation/ ,a))))))

(defmacro b\\ (a &optional b)
  "Solve b x = a (/b a); or compute /a"
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:/ ,a ,b))
	 ((number null) (cl:/ ,a))
	 ((tensor number) (div! ,b ,a))
	 (((or tensor-vector tensor-matrix) tensor-square-matrix)
	  (let ((m:*default-stride-ordering* :col-major))
	    (getrs! (getrf! (copy ,b)) ,a)))
	 ((tensor-square-matrix null) (getri! (getrf! (copy ,a))))
	 ((tensor permutation) (permute! ,a (permutation/ ,b) 0))
	 ;;The correctness of this depends on the left-right order in reduce (foldl).
	 ((permutation permutation) (permutation* (permutation/ ,b) ,a))
	 ((permutation null) (permutation/ ,a))))))

(definline / (object &rest more-objects)
  (if more-objects
      (let ((ret (b+ object)))
	(iter (for fst in more-objects) (setf ret (b/ ret fst)))
	ret)
      (b/ object)))

(define-compiler-macro / (object &rest more-objects)
  (if more-objects
      (rec reducer ((ret `(b+ ,object)) (objs more-objects))
	   (if (not objs) ret
	       (reducer `(b/ ,ret ,(car objs)) (cdr objs))))
      `(b/ ,object)))
;;
(defmacro b= (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:= ,a ,b))	 
	 (((or tensor number) tensor) (ga= ,a ,b))
	 ((tensor number) (ga= ,b ,a))
	 (((or number tensor) null) t)))))

(definline = (object &rest more-objects)
  (iter (for fst in more-objects)
	(unless (b= object fst) (return nil))
	(finally (return t))))

(define-compiler-macro = (object &rest more-objects)
  (with-gensyms (o)
    `(let ((,o ,object))
       (and ,@ (mapcar #'(lambda (x) `(b= ,o ,x)) more-objects)))))
;;
;; (defmacro b.= (a &optional b)
;;   (using-gensyms (decl (a b))
;;     `(let (,@decl)
;;        (cart-etypecase (,a ,b)
;; 	 ((number number) (cl:= ,a ,b))	 
;; 	 (((or tensor number) tensor) (ge= ,a ,b))
;; 	 ((tensor number) (ge= ,b ,a))
;; 	 (((or number tensor) null) t)))))

;; (definline .= (object &rest more-objects)
;;   (iter (for fst in more-objects)
;; 	(unless (b.= object fst) (return nil))
;; 	(finally (return t))))

;; (define-compiler-macro .= (object &rest more-objects)
;;   (with-gensyms (o)
;;     `(let ((,o ,object))
;;        (and ,@ (mapcar #'(lambda (x) `(b.= ,o ,x)) more-objects)))))
;;
(defmacro b⊗ (a &optional b)
  (using-gensyms (decl (a b))
    `(let (,@decl)
       (cart-etypecase (,a ,b)
	 ((number number) (cl:* ,a ,b))
	 ((number null) ,a)
	 ((tensor number) (orphanize (suptensor~ (scal! ,b ,a) (1+ (order ,a)))))
	 ((number tensor) (orphanize (suptensor~ (scal ,a ,b) (1+ (order ,b)) 1)))
	 ((tensor-vector tensor-vector) (ger 1 ,a ,b nil nil))
	 ((tensor tensor) (gekr! 1 ,a ,b 1 (zeros (append (dimensions ,a t) (dimensions ,b t)) (class-of ,a))))
	 ((tensor null) (b+ ,a))))))

(definline ⊗ (object &rest more-objects &aux (ret (b⊗ object)))
  (iter (for fst in more-objects) (setf ret (b⊗ ret fst)))
  ret)

(define-compiler-macro ⊗ (object &rest more-objects)
  (rec reducer ((ret `(b⊗ ,object)) (objs more-objects))
       (if (not objs) ret
	   (reducer `(b⊗ ,ret ,(car objs)) (cdr objs)))))
;;
;; (definline realpart~ (object) (tensor-realpart~ object))
;; (definline realpart (object) (tensor-realpart object))
;; (definline imagpart~ (object) (tensor-imagpart~ object))
;; (definline imagpart (object) (tensor-imagpart object))
;; (definline sum! (x y &optional axis) (tensor-sum! x y axis))
;; (definline sum (x &optional axis preserve-rankp) (tensor-sum x axis preserve-rankp))


;;
