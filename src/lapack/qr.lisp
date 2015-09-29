(in-package #:matlisp)

;;
(deft/generic (t/lapack-geqp! #'subtypep) sym (A lda jpvt tau))
(deft/method t/lapack-geqp! (sym blas-mixin) (A lda jpvt tau)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda jpvt tau) (xxx xxr lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt)
		  (type ,(store-type sym) ,tau))
	 (with-field-elements ,sym (,@(when complex? `((,xxr (t/fid+ ,ftype) (dimensions ,A 1)))))
	   (with-lapack-query ,sym (,xxx ,lwork)
	     (ffuncall ,(blas-func "geqp3" ftype)
	       (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1)	     
	       (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	       (:* :integer) (the (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*)) ,jpvt)
	       (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	       (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	       ,@(when complex? `((:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxr)))
	       (:& :integer :output) 0)))))))

(deft/generic (t/lapack-gehr! #'subtypep) sym (A lda tau))
(deft/method t/lapack-gehr! (sym blas-mixin) (A lda tau)
  (let* ((ftype (field-type sym)))
    (using-gensyms (decl (A lda tau) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type ,(store-type sym) ,tau))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func "geqrf" ftype)
	     (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1)
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))
;;
(defgeneric qr! (a &optional pivot?)
  (:method :before ((a dense-tensor) &optional pivot?)
     (declare (ignore pivot?))
     (assert (tensor-matrixp a) nil 'tensor-not-matrix)))

(define-tensor-method qr! ((a blas-mixin :x t) &optional pivot?)
  `(let-typed ((tau (zeros (lvec-min (dimensions a)) ',(cl a)) :type ,(cl a)))
     (if pivot?
	 (let-typed ((jpvt (make-array (dimensions a 1) :element-type ',(matlisp-ffi::%ffc->lisp :integer) :initial-element 0) :type (simple-array ,(matlisp-ffi::%ffc->lisp :integer) (*))))
	   (with-columnification (() (a))
	     (let ((info (t/lapack-geqp! ,(cl a) a (or (blas-matrix-compatiblep a #\N) 0) jpvt (store tau))))
	       (unless (= info 0) (error "GEQP3: the ~a'th argument had an illegal value." (- info)))))
	   (setf (gethash 'geqp3 (memos A)) (list (store tau) jpvt))
	   (values A tau (with-no-init-checks (make-instance 'permutation-action :store (pflip.f->l jpvt) :size (length jpvt)))))
	 (progn
	   (with-columnification (() (a))
	     (let ((info (t/lapack-gehr! ,(cl a) a (or (blas-matrix-compatiblep a #\N) 0) (store tau))))
	       (unless (= info 0) (error "GEQRF: the ~a'th argument had an illegal value." (- info)))))
	   (setf (gethash 'geqrf (memos A)) (store tau))
	   (values A tau)))))

;;
(deft/generic (t/lapack-orgqr! #'subtypep) sym (rank A lda tau))
(deft/method t/lapack-orgqr! (sym blas-mixin) (rank A lda tau)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda tau rank) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda ,rank)
		  (type ,(store-type sym) ,tau))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if complex? "ungqr" "orgqr") ftype)
	     (:& :integer) (dimensions ,A 0) (:& :integer) (dimensions ,A 1) (:& :integer) ,rank
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))

(defgeneric qr (a &optional pivot?))

(define-tensor-method qr ((a blas-mixin :x t) &optional pivot?)
  `(letv* ((qr tau p (qr! (copy a) pivot?))
	   (qq (zeros (list (dimensions a 0) (dimensions a 0)) ',(cl a)))
	   (rank (lvec-min (dimensions qr))))
     (copy! (subtensor~ qr (list (list 0 nil) (list 0 rank))) (subtensor~ qq (list (list 0 nil) (list 0 rank))))
     (with-columnification (() (qq))
       (let ((info (t/lapack-orgqr! ,(cl a) rank qq (or (blas-matrix-compatiblep qq #\N) 0) (store tau))))
	 (unless (= info 0) (error "(OR/UN)GQR: the ~a'th argument had an illegal value." (- info)))))
     (values-n (if p 3 2) qq (tricopy! 0 qr :lo) p)))
;;
#+nil
(defun gram-schmidt (Q k)
  (iter (for qi slicing Q along -1 from k with-index i)
	(iter (for qj slicing Q along -1 from 0 below i) (axpy! (- (dot qj qi)) qj qi))
	(normalize! qi 2)))

;;
(deft/generic (t/lapack-ormqr! #'subtypep) sym (side trans rank A lda tau c ldc))
(deft/method t/lapack-ormqr! (sym blas-mixin) (side trans rank A lda tau c ldc)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (side trans A lda tau c ldc rank) (xxx lwork))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda ,ldc ,rank)
		  (type ,(store-type sym) ,tau)
		  (type character ,side ,trans))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if complex? "unmqr" "ormqr") ftype)
	     (:& :character) ,side (:& :character) ,trans
	     (:& :integer) (dimensions ,C 0) (:& :integer) (dimensions ,C 1) (:& :integer) ,rank
	     (:* ,(lisp->ffc ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :integer) ,lda
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,tau)
	     (:* ,(lisp->ffc ftype) :+ (head ,C)) (the ,(store-type sym) (store ,C)) (:& :integer) ,ldc
	     (:* ,(lisp->ffc ftype)) (the ,(store-type sym) ,xxx) (:& :integer) ,lwork
	     (:& :integer :output) 0))))))

;;(defgeneric geqrs! (a tau b))
#+nil
(defun house (a b)
  (letv* ((q tau (geqr! (copy a))))
    (t/lapack-ormqr! real-tensor #\L #\T (lvec-min (dimensions q)) q (or (blas-matrix-compatiblep q) 0) tau b (or (blas-matrix-compatiblep b) 0))
    (t/blas-trsm! real-tensor #\L #\U #\N #\N 1d0 q (or (blas-matrix-compatiblep q) 0) b (or (blas-matrix-compatiblep b) 0))
    )
  b)
