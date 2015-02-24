(in-package #:matlisp)

;;The scheme for this iterator was obtained from FEMLISP.
(defmacro-clause (FOR-MOD idx FROM initial BELOW dimensions &optional WITH-STRIDES strides LOOP-ORDER order UPLO ul)
  (check-type idx symbol)
  (let ((ul (or ul :ul))
	(order (or order (case ul ((:u :uo) :col-major) ((:l :lo) :row-major)) matlisp::*default-stride-ordering*)))
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
					       (with ,(first x) = (+ ,(or (third x) 0)
								     ,@(unless (and (numberp initial) (zerop initial))
									 `((iter (for ,(gy 'i) from 0 below (length ,init))
										 (summing (the index-type (* (aref ,(gy (first x)) ,(gy 'i)) (aref ,init ,(gy 'i)))) into ,(gy 'ihead))
										 (declare (type index-type ,(gy 'ihead) ,(gy 'i)))
										 (finally (return ,(gy 'ihead))))))))))
			       strides)
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
			     (:lo `(or (and (> ,count 0) (= (aref ,idx ,count) (1- (aref ,idx (1- ,count)))))))
			     (:u `(or (and (< ,count (1- (length ,idx))) (= (aref ,idx ,count) (aref ,idx (1+ ,count))))))
			     (:uo `(or (and (< ,count (1- (length ,idx))) (= (aref ,idx ,count) (1- (aref ,idx (1+ ,count))))))))
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
	 (ssyms (mapcar #'(lambda (x y) (when y `(,(gensym) (slot-value ,(car x) 'store)))) tsyms types))
	 (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    (using-gensyms (decl (dims) ())
      `(let-typed (,@decl
		   ,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
	 (declare (type index-store-vector ,dims))
	 (let-typed (,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
	   (iter (for-mod ,idx from ,(case uplo?
					   (:uo `(append (make-list (1- (length ,dims)) :initial-element 0) (list 1)))
					   (:lo `(append (list 1) (make-list (1- (length ,dims)) :initial-element 0)))
					   (t 0))
			  below ,dims with-strides (,@(remove-if #'null (mapcar #'(lambda (of ten typ) (when typ `(,of (strides ,(car ten)) (head ,(car ten)))))
										osyms tsyms types)))
			  ,@(when loop-ordering-p `(loop-order ,loop-order)) uplo ,uplo?)
		 (symbol-macrolet (,@(mapcar #'(lambda (ref sto ten of typ) (if typ
										(list ref `(the ,(field-type typ) (t/store-ref ,typ ,(car sto) ,of)))
										(list ref `(ref ,(car ten) ,idx))))
					     rsyms ssyms tsyms osyms types))
		   ,@body)))))))
