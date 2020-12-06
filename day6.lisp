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

(defun sum-counts ()
  (loop for group in (load-data)
	  sum (count-yes-questions group)))
