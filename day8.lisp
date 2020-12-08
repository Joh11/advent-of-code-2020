(defpackage #:aoc2020/8
  (:use #:cl)
  (:export
   #:compute-accumulator-value
   #:compute-accumulator-value-when-halts))

(in-package #:aoc2020/8)

(defun parse-line (line)
  (let* ((tokens (ppcre:split " " line))
	 (op (cond
	       ((string= (first tokens) "acc") 'acc)
	       ((string= (first tokens) "jmp") 'jmp)
	       ((string= (first tokens) "nop") 'nop)
	       (t (error "Invalid op code"))))
	 (arg (parse-integer (second tokens))))
    (list op arg)))

(defun load-data (&optional (path "data/day8.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-line line) into ops
	  finally (return (make-array (length ops) :initial-contents ops)))))

(defun compute-accumulator-value (ops)
  (loop with acc = 0
	  with execp = (make-array (length ops) :initial-element nil)
	  with i = 0
	  when (>= i (length ops)) return (values acc t)
	  when (aref execp i) return (values acc nil) do
	    (format t "~a: ~a, acc=~a ~%" i (aref ops i) acc)
	    (setf (aref execp i) t)
	    (case (first (aref ops i))
	      (acc (progn (incf acc (second (aref ops i)))
			  (incf i)))
	      (jmp (incf i (second (aref ops i))))
	      (nop (incf i)))))

(defun compute-accumulator-value-when-halts ()
  (let ((ops (load-data)))
    ;; First check it without changing anything
    (multiple-value-bind (acc haltp) (compute-accumulator-value ops)
      (when haltp (return-from compute-accumulator-value-when-halts acc)))
    ;; Then try changing ops
    (loop for i from 0 below (length ops)
	  when (or (eq (car (aref ops i)) 'nop)
		   (eq (car (aref ops i)) 'jmp))
	    do
	       ;; Change it
	       (if (eq (car (aref ops i)) 'nop)
		   (setf (car (aref ops i)) 'jmp)
		   (setf (car (aref ops i)) 'nop))
	       ;; Check if halts
	       (multiple-value-bind (acc haltp) (compute-accumulator-value ops)
		 (when haltp (return acc)))
	       ;; Revert if it doesn't
	       (if (eq (car (aref ops i)) 'nop)
		   (setf (car (aref ops i)) 'jmp)
		   (setf (car (aref ops i)) 'nop)))))
