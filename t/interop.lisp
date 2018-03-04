(in-package #:matlisp-tests)

(fiveam:in-suite matlisp-tests:matlisp-tests)

(5am:test as-tensor-test
  ;; Test simple conversion of 1D vectors
  (let ((a (as-tensor #(1 2 3))))
    (is (dimensions a) #(3))
    (is (= (ref a 1) 2))
    (is (equal (array-element-type (slot-value a 'store)) t)))

  ;; Tensor to tensor same object
  (let* ((a (ones '(2 3))) ; a is a tensor
         (b (as-tensor a)))
    (is (eq a b))) ; Same object

  ;; Conversion of 2D arrays
  (let* ((a (make-array '(2 3) :initial-element 2.5 :element-type 'single-float))
         (b (as-tensor a)))
    (is (equalp (dimensions b) #(2 3)))
    (is (< (abs (- (ref b 1 2) 2.5)) 1e-8))
    (is (equal (array-element-type (slot-value b 'store)) 'single-float)))

  (let ((a (as-tensor #2A((1 2 3)
                          (4 5 6)))))
    (is (= (ref a 1 1) 5))))


(5am:test as-array-test
  ;; Array not modified
  (let ((a #(1 2 3)))
    (is (eq a (as-array a))))

  ;; 1D tensor
  (let* ((a (range 0 4))
         (b (as-array a)))
    (is (= (length b) 4))
    (is (= (aref b 1) 1)))

  ;; 2D tensor

  (let* ((a (as-tensor #2A((1 2 3)
                           (4 5 6))))
         (b (as-array a)))
    (is (= (array-rank b) 2))
    (is (= (aref b 1 0) 4))
    (is (= (aref b 0 2) 3))))

