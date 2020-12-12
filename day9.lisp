(defpackage #:aoc2020/9
  (:use #:cl)
  (:export
   #:find-first-weakness
   #:find-encryption-weakness))

(in-package #:aoc2020/9)

(defun load-data (&optional (path "data/day9.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-integer line) into ls
	  finally (return (make-array (length ls) :initial-contents ls)))))

(defun check-correctness (n xs)
  (loop for i from 0 below (length xs)
	when (loop for j from (1+ i) below (length xs)
		   when (= n (+ (aref xs i) (aref xs j)))
		     return t
		   finally (return nil))
	  return t
	finally (return nil)))

(defun find-first-weakness ()
  (let ((numbers (load-data)))
    (loop for i from 25 below (length numbers)
	  unless (check-correctness (aref numbers i)
				    (subseq numbers (- i 25) i))
	    return (aref numbers i))))

(defun find-contiguous-range ()
  (let ((numbers (load-data))
	(target (find-first-weakness)))
    (loop for i from 0 below (1- (length numbers))
	  collect
	  (loop for j from (1+ i) below (length numbers)
		with sum = (aref numbers i) do
		  (setf sum (+ sum (aref numbers j)))
		  (cond
		    ((= sum target) (return (subseq numbers i (1+ j))))
		    ((> sum target) (return nil))))
	    into ranges
	  finally (return (first (remove-if #'null ranges))))))

(defun find-encryption-weakness ()
  (let* ((range (find-contiguous-range))
	 (min (reduce #'min range))
	 (max (reduce #'max range)))
    (+ min max)))
