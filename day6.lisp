(defpackage #:aoc2020/6
  (:use #:cl)
  (:export
   ))

(in-package #:aoc2020/6)

(defun load-data (&optional (path "data/day6.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil) while line
	  with groups = (list nil) do
	    (cond
	      ((string= line "") (push nil groups))
	      (t (push line (car groups))))
	  finally (return groups))))

(defun count-yes-questions (group)
  (loop for person in group
	with hash = (make-hash-table :test 'equal) do
	  (loop for answer across person do
	    (setf (gethash answer hash) t))
	finally (return (hash-table-count hash))))

(defun count-all-yes-questions (group)
  (loop for answer across (car group)
	when (= 1 (length group))
	  return (length (car group))
	count (every (lambda (answers) (member answer (coerce answers 'list))) (cdr group))))

(defun sum-counts (&key everyone)
  (loop for group in (load-data)
	sum (if everyone
		(count-all-yes-questions group)
		(count-yes-questions group))))
