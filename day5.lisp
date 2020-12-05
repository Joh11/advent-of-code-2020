(defpackage #:aoc2020/5
  (:use #:cl)
  (:export
   #:highest-seat-id))

(in-package #:aoc2020/5)

(defun load-data (&optional (path "data/day5.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun str-to-binary (str zero one)
  (loop for c across str
	unless (or (char= c zero) (char= c one))
	  do (error "Invalid character")
	collect (char= c one)))

(defun binary-to-num (bin-list)
  (loop for d in (reverse bin-list)
	for i from 0
	sum (* (expt 2 i) (if d 1 0))))

(defun pass-str-to-seat (str)
  (let ((bin-row (str-to-binary (subseq str 0 7) #\F #\B))
	(bin-col (str-to-binary (subseq str 7) #\L #\R)))
    (values (binary-to-num bin-row)
	    (binary-to-num bin-col))))

(defun highest-seat-id ()
  (let ((passes (load-data)))
    (loop for pass in passes
	  maximize
	  (multiple-value-bind (r c)
	      (pass-str-to-seat pass)
	    (+ (* r 8) c)))))
