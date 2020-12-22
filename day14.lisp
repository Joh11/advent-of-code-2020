(defpackage #:aoc2020/14
  (:use #:cl)
  (:export
   ))

(in-package #:aoc2020/14)

(defun parse-mask (line)
  (let ((mask 0)
	(onemask 0)
	(n (1- (length line))))
    (dotimes (k (1+ n))
      (cond
	((char= (aref line k) #\X)) ;; do nothing
	((char= (aref line k) #\0) (setf mask (logior mask (ash 1 (- n k)))))
	((char= (aref line k) #\1) (progn
				     (setf mask (logior mask (ash 1 (- n k))))
				     (setf onemask (logior onemask (ash 1 (- n k))))))
	(t (error "Invalid character"))))
    (list mask onemask)))

(defun mask-string (mask onemask)
  (loop for k from 0 below 36
	collect
	(if (/= 0 (logand mask (ash 1 (- 36 k))))
	    (if (/= 0 (logand onemask (ash 1 (- 36 k))))
		#\1
		#\0)
	    #\X)
	  into xs
	finally (return (coerce xs 'string))))

(defun parse-mem-write (line)
  (multiple-value-bind (res groups)
      (ppcre:scan-to-strings "^mem\\[(\\d+)\\] = (\\d+)$" line)
    (list (parse-integer (aref groups 0))
	  (parse-integer (aref groups 1)))))

(defun parse-line (line)
  (if (char= (aref line 1) #\a)
      ;; mask
      (destructuring-bind (mask onemask)
	  (parse-mask (subseq line 7))
	(list :mask mask onemask))
      ;; memory write
      (destructuring-bind (addr val)
	  (parse-mem-write line)
	(list :mem addr val))))

(defun load-data (&optional (path "data/day14.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-line line))))

(defun part1 ()
  (let ((instrs (load-data))
	(mask 0)
	(onemask 0)
	(mem (make-hash-table)))
    (loop for instr in instrs do
      (if (eq (first instr) :mask)
	  ;; Set mask
	  (progn
	    (setf mask (second instr))
	    (setf onemask (third instr)))
	  ;; Set memory
	  (setf (gethash (second instr) mem)
		(logior (logand mask onemask)
			(logand (lognot mask) (third instr))))))
    (let ((count 0))
      (maphash (lambda (k v)
		 (declare (ignore k))
		 (incf count v))
	       mem)
      count)))
