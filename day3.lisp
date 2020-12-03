(in-package #:advent-of-code-2020)

(defun load-data-day3 (&optional (path "data/day3.dat"))
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

(defun count-clashing-trees ()
  (let* ((map (load-data-day3))
	 (height (array-dimension map 0))
	 (width (array-dimension map 1)))
    (loop for i below height
	  for j = (rem (* 3 i) width)
	  count (aref map i j))))
