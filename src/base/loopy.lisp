(in-package #:matlisp)

(defmacro-clause (FOR-MOD idx FROM initial BELOW dimensions &optional WITH-STRIDES strides LOOP-ORDER order UPLO ul)
  (check-type idx symbol)
  (let ((ul (or ul :ul))
	(order (or order (and ul (case ul (:u :col-major) (:l :row-major))) *default-stride-ordering*)))
    (binding-gensyms (#.(gensym) gy)
      (with-gensyms (dims init count)
	`(progn
	   (with ,dims = (coerce ,dimensions 'index-store-vector))
	   (with ,init = (let ((,idx ,initial))
			   (if (numberp ,idx)
			       (t/store-allocator index-store-vector (length ,dims) ,idx)
			       (coerce ,initial 'index-store-vector))))
	   (with ,idx = (copy-seq ,init))
	   (declare (type index-store-vector ,dims ,idx ,init))
	   ;;
	   ,@(when strides
		   `(,@(mapcan #'(lambda (x) `((with ,(gy (first x)) = ,(second x))
					       (with ,(first x) = ,(or (third x) 0)))) strides)
		       (declare (type index-store-vector ,@(mapcar #'(lambda (x) (gy (first x))) strides))
				(type index-type ,@(mapcar #'car strides)))))
	   ;;
	   (initially (assert (ziprm (= length) (,dims ,init ,@(when strides (mapcar #'(lambda (x) (gy (first x))) strides)))) nil "Rank mismatch."))
	   (after-each
	    (unless
		(very-quickly
		  ;;For some odd reason loop does a better job here!		  
		  (,@(ecase order
		       (:row-major `(loop :for ,count :of-type index-type :from (1- (length ,idx)) :downto 0))
		       (:col-major `(loop :for ,count :of-type index-type :from 0 :below (length ,idx)))) :do
		     (if ,(recursive-append
			   (ecase ul
			     (:ul nil)
			     (:l `(or (and (> ,count 0) (= (aref ,idx ,count) (aref ,idx (1- ,count))))))
			     (:u `(or (and (< ,count (1- (length ,idx))) (= (aref ,idx ,count) (aref ,idx (1+ ,count)))))))
			   `(= (1+ (aref ,idx ,count)) (aref ,dims ,count)))
			 (progn
			   ,@(when strides (mapcar #'(lambda (x) `(decf ,(first x) (the index-type (* (aref ,(gy (first x)) ,count) (- (aref ,idx ,count) (aref ,init ,count)))))) strides))
			   (setf (aref ,idx ,count) (aref ,init ,count)))
			 (progn
			   ,@(when strides (mapcar #'(lambda (x) `(incf ,(first x) (aref ,(gy (first x)) ,count))) strides))
			   (incf (aref ,idx ,count))
			   (return t)))))
	      (finish))))))))

(defmacro dorefs ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) (&rest ref-decls) &rest body)
  (let* ((tsyms (zipsym (mapcar #'second ref-decls)))
	 (rsyms (mapcar #'car ref-decls))
	 (types (mapcar #'(lambda (x) (destructuring-bind (ref ten &key type) x
					(declare (ignore ref ten))
					type))
			ref-decls))
	 (ssyms (mapcar #'(lambda (x y) (when y `(,(gensym) (store ,(car x))))) tsyms types))
	 (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    `(let-typed (,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
       (let-typed (,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
	 (mod-dotimes (,idx ,dims ,@(when loop-ordering-p `(:loop-order ,loop-order)) :uplo? ,uplo?)
	   :with (linear-sums
		  ,@(remove-if #'null (mapcar #'(lambda (of ten typ) (when typ `(,of (strides ,(car ten)) (head ,(car ten)))))
					      osyms tsyms types)))
	   :do (symbol-macrolet (,@(mapcar #'(lambda (ref sto ten of typ) (if typ
									      (list ref `(the ,(field-type typ) (t/store-ref ,typ ,(car sto) ,of)))
									      (list ref `(ref ,(car ten) ,idx))))
					   rsyms ssyms tsyms osyms types))
		 ,@body))))))
