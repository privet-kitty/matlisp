(in-package #:matlisp-utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)  

(definline lvec-foldl (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :from 0 :below (length vec)
     :for ret = (aref vec 0) :then (funcall func ret (aref vec i))
     :finally (return ret)))

(definline lvec-foldr (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :downfrom (1- (length vec)) :to 0
     :for ret = (aref vec (1- (length vec))) :then (funcall func (aref vec i) ret)
     :finally (return ret)))

(definline lvec-map-foldl! (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :from 0 :below (length vec)
     :for ret = (aref vec 0) :then (funcall func (aref vec i) ret)
     :do (setf (aref vec i) ret)
     :finally (return (values ret vec))))

(definline lvec-map-foldr! (func vec)
  (declare (type vector))
  (loop
     :for i :of-type fixnum :downfrom (1- (length vec)) :to 0
     :for ret = (aref vec (1- (length vec))) :then (funcall func (aref vec i) ret)
     :do (setf (aref vec i) ret)
     :finally (return (values ret vec))))

(definline lvec-max (vec)
  (declare (type vector vec))
  (loop :for ele :across vec
     :for idx :of-type fixnum = 0 :then (+ idx 1)
     :with max :of-type fixnum = (aref vec 0)
     :with max-idx :of-type fixnum = 0
     :do (when (> ele max)
	   (setf max ele
		 max-idx idx))
     :finally (return (values max max-idx))))

(definline lvec-min (vec)
  (declare (type vector vec))
  (loop :for ele :across vec
     :for idx :of-type fixnum = 0 :then (+ idx 1)
     :with min :of-type fixnum = (aref vec 0)
     :with min-idx :of-type fixnum = 0
     :do (when (< ele min)
	   (setf min ele
		 min-idx idx))
     :finally (return (values min min-idx))))

(definline lvec-eq (va vb &optional (test #'eq))
  (declare (type vector va vb))
  (let ((la (length va))
	(lb (length vb)))
    (if (/= la lb) nil
	(loop
	   :for ele-a :across va
	   :for ele-b :across vb
	   :unless (funcall test ele-a ele-b)
	   :do (return nil)
	   :finally (return t)))))

(definline lvec->list (va)
  (declare (type vector va))
  (loop :for ele :across va
     :collect ele))

(definline lvec->list! (va la)
  (declare (type vector va)
	   (type list la))
  (loop
     :for ele :across va
     :for lst = la :then (cdr lst)
     :do (setf (car lst) ele))
  la)

(let ((code `(lambda (n in of/in out of/out &key key lock)
	       (declare (type fixnum n of/in of/out)
			(type simple-array in)
			(type simple-array out))
	       (let ((key (or key #'row-major-aref))
		     (lock (or lock #'(setf row-major-aref))))
		 (declare (ignorable key lock))
		 (loop :for i :of-type fixnum :from 0 :below n
		    :do (funcall lock (funcall key in (the fixnum (+ of/in i))) out (the fixnum (+ of/out i))))
		 out))))
  (setf (symbol-function 'lvec-copy) (compile nil code)
	(documentation 'lvec-copy 'function) "")
  ;;
  (define-compiler-macro lvec-copy (&whole form n in of/in out of/out &key (key '(function row-major-aref)) (lock '(function (setf row-major-aref))))
    `(let (,@(zip (subseq (second code) 0 5) (cdr form))
	   (key ,key) (lock ,lock))
       (declare (ignorable key lock))
       ,@(maptree-eki #'(lambda (x)
			  (match x
			    ((list 'type _ (and v-name (or 'in 'out))) (match (ecase v-name (in in) (out out)) ((list 'the type _) `(type ,type ,v-name)) (_ x)))
			    ((list* 'funcall 'key argvs)
			     (match key
			       ((list (or 'function 'quote) (and f (type symbol))) `(,f ,@argvs))
			       ((or (list 'function (list* 'lambda (and args (list _ _)) body)) (list* 'lambda (and args (list _ _)) body))
				`(let (,@(zip args argvs)) ,@body))
			       (_ x)))
			    ((list* 'funcall 'lock argvs)
			     (match lock
			       ((list (or 'function 'quote) (and f (type symbol))) `(,f ,@argvs))
			       ((list (or 'function 'quote) (list 'setf (and f (type symbol)))) (values `(setf (,f ,@(cdr argvs)) ,(car argvs)) t))
			       ((or (list 'function (list* 'lambda (and args (list _ _ _)) body)) (list* 'lambda (and args (list _ _ _)) body))
				(values `(let (,@(zip args argvs)) ,@body) t))
			       (_ x)))
			    (_ (values x t))))
		      (cddr code)))))
)
