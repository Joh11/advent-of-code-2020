(defpackage #:aoc2020/3
  (:use #:cl)
  (:export
   #:count-clashing-trees
   #:product-clashing-trees))

(in-package #:aoc2020/3)

(defun load-data (&optional (path "data/day3.dat"))
  (with-open-file (stream path)
    (let* ((lines (loop for line = (read-line stream nil)
			while line
			collect line))
	   (height (length lines))
	   (width (length (first lines)))
	   (arr (make-array `(,height ,width) :initial-element nil)))
      (loop for i below height
	    for line in lines do
	      (loop for j below width do
		(when (char= #\# (elt line j))
		  (setf (aref arr i j) t))))
      arr)))

(defun count-clashing-trees (&optional (step-right 3) (step-down 1))
  (let* ((map (load-data))
	 (height (array-dimension map 0))
	 (width (array-dimension map 1)))
    (loop for k from 1
	  for i = (* step-down k)
	  for j = (rem (* step-right k) width)
	  while (< i height)
	  count (aref map i j))))

(defun product-clashing-trees ()
  (reduce #'* (list
	       (count-clashing-trees 1 1)
	       (count-clashing-trees 3 1)
	       (count-clashing-trees 5 1)
	       (count-clashing-trees 7 1)
	       (count-clashing-trees 1 2))))
