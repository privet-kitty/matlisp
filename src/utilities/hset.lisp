(in-package #:matlisp-hset)

(defun list->hset (lst &optional (test 'eql) &aux (table (make-hash-table :test test)))
  (iter (for xi in lst) (setf (gethash xi table) t))
  table)

(defun hset->list (hset)
  (if hset (iter (for (k v) in-hashtable hset) (collect k))))

(defun hset-memberp (element table)
  (nth-value 1 (gethash element table)))

(defun hset-add! (element table)
  (gethash! element table t)
  table)

(defun hset-rem! (element table)
  (remhash element table)
  table)

(defmacro iter-hset-lst ((k s) &body body)
  (alexandria:once-only (s)
    `(etypecase ,s
       (list (iter (for ,k in ,s) (progn ,@body)))
       (hash-table (iter (for (,k ,(gensym "v")) in-hashtable ,s) (progn ,@body))))))

(defun ensure-hset (tbl &optional (test 'eql))
  (etypecase tbl
    (list (list->hset tbl test))
    (hash-table tbl)))

(defun hset-union! (tbl1 tbl2 &aux (tbl1 (ensure-hset tbl1)))
  (iter-hset-lst (k tbl2) (hset-add! k tbl1))
  tbl1)

(defun hset-intersection! (tbl1 tbl2 &aux (tbl1 (ensure-hset tbl1)))
  (iter-hset-lst (k tbl1)
    (unless (hset-memberp k tbl2)
      (hset-rem! k tbl1)))
  tbl1)

(defun hset-difference! (tbl1 tbl2 &aux (tbl1 (ensure-hset tbl1)))
  (iter-hset-lst (k tbl2)
    (hset-rem! k tbl1))  
  tbl1)
