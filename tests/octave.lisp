(in-package :matlisp)
(defvar *current-octave-process* nil)

(defun open-octave-stream (&key (octave-binary "/usr/bin/octave")
			   &aux (process (setf *current-octave-process* (sb-ext:run-program #+nil external-program:run octave-binary '("--no-gui" "--silent") :input :stream :wait nil :output :stream))))
  (octave-send "format long E; 1 ~%")
  (let ((stream (external-program:process-output-stream process)))
    (loop :repeat 1000
       :do (let ((out (car (split-seq #'(lambda (x) (char= x #\Space)) (print (read-line stream))))))
	     (when (string= out "ans") (return process))))))

(defun close-octave-stream ()
  (when *current-octave-process*
    (octave-send "quit~%")
    (setf *current-octave-process* nil)))
;;
(defun octave-send (str &rest args &aux (process (or *current-octave-process* (setf *current-octave-process* (open-octave-stream)))))
  (let ((stream (external-program:process-input-stream process)))
    (apply #'format (append (list stream str) args))
    (finish-output stream)))

(defun octave-clear () (octave-send "clear~%"))

(defun octave-readnum (&aux (process *current-octave-process*))
  (let ((stream (external-program:process-output-stream process)))
    (let ((str (split-seq #'(lambda (c) (char= c #\Space)) (read-line stream))))
      (cond
	((= (length str) 5)
	 (let ((real (third str))
	       (imag (fifth str)))
	   (setf (aref real (position #\E real)) #\D)
	   (setf (aref imag (position #\E imag)) #\D)
	   (setf imag (subseq imag 0 (1- (length imag))))
	   (if (string= (fourth str) "+")
	       (complex (read-from-string real) (read-from-string imag))
	       (complex (read-from-string real) (- (read-from-string imag))))))
	((= (length str) 3)
	 (let ((real (third str)))
	   (setf (aref real (position #\E real)) #\D)
	   (nth-value 0 (read-from-string real))))))))

(defun octave-send-tensor (mat name)
  (octave-send "~a = zeros(~{~a~^, ~});~%" name (if (> (order mat) 1) (dimensions mat t) (append (dimensions mat t) (list 1))))
  (iter (for-mod idx from 0 below (dimensions mat))
	(let ((ref (apply #'ref (list* mat (lvec->list idx)))))
	  (if (complexp ref)
	      (octave-send "~a(~{~a~^, ~}) = ~a + ~a * 1i;~%" name (mapcar #'1+ (lvec->list idx)) (realpart ref) (imagpart ref))
	      (octave-send "~a(~{~a~^, ~}) = ~a;~%" name (mapcar #'1+ (lvec->list idx)) ref)))))

(defun octave-read-tensor (com &aux (process *current-octave-process*) (name (symbol-name (gensym "_tmp"))))
  (octave-send (format nil "~a = ~a;~%" name com))
  (let ((stream (external-program:process-output-stream process))
	dims ret complex?)
    (clear-input stream)
    ;;
    (octave-send "size(~a)~%" name)
    (read-line stream) (read-line stream)
    (let ((out (mapcar
		#'(lambda (x) (setf (aref x (position #\E x)) #\D) (ceiling (read-from-string x)))
		(split-seq #'(lambda (c) (char= c #\Space)) (read-line stream)))))
      (read-line stream)
      (setq dims out))
    ;;
    (octave-send "sum(abs(imag(~a(:))))~%" name)
    (setq ret
	  (if (= (octave-readnum) 0d0)
	      (zeros dims (tensor 'double-float))
	      (prog1 (zeros dims (tensor '(complex double-float))) (setq complex? t))))
    ;;
    (iter (for-mod idx from 0 below (dimensions ret))
	  (setf (apply #'ref (list* ret (lvec->list idx))) (progn (octave-send "~a(~{~a~^, ~})~%" name (mapcar #'1+ (lvec->list idx))) (octave-readnum))))
    ret))
