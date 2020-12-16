(defpackage #:aoc2020/11
  (:use #:cl #:alexandria)
  (:export
   ))

(in-package #:aoc2020/11)

(defun load-data (&optional (path "data/day11.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (mapcar (lambda (x)
			    (cond
			      ((char= x #\L) :empty)
			      ((char= x #\#) :occupied)
			      ((char= x #\.) :floor)
			      (t (error "wrong data"))))
			  (coerce line 'list)) into ls
	  finally (return (make-array (list (length ls) (length (first ls)))
				      :initial-contents ls)))))

(defun count-occupied-seats (seats row col)
  (let ((neighbors nil))
    ;; Populate the neighbors list
    (when (> row 0)
      (push `(,(1- row) ,col) neighbors))
    (when (> col 0)
      (push `(,row ,(1- col)) neighbors))
    (when (< row (1- (array-dimension seats 0)))
      (push `(,(1+ row) ,col) neighbors))
    (when (< col (1- (array-dimension seats 1)))
      (push `(,row ,(1+ col)) neighbors))

    (when (and (> row 0) (> col 0))
      (push `(,(1- row) ,(1- col)) neighbors))
    (when (and (> row 0) (< col (1- (array-dimension seats 0))))
      (push `(,(1- row) ,(1+ col)) neighbors))
    (when (and (< row (1- (array-dimension seats 0))) (> col 0))
      (push `(,(1+ row) ,(1- col)) neighbors))
    (when (and (< row (1- (array-dimension seats 0))) (< col (1- (array-dimension seats 1))))
      (push `(,(1+ row) ,(1+ col)) neighbors))

    
    (loop for (nr nc) in neighbors count
	  (eq (aref seats nr nc) :occupied))))

(defun update-seats (seats)
  "Returns the new seats, and t if it has been updated. "
  (loop for row from 0 below (array-dimension seats 0)
	with modifiedp = nil
	with ret = (copy-array seats) do
	  (loop for col from 0 below (array-dimension seats 1) do
	    (let ((state (aref seats row col))
		  (noccupied (count-occupied-seats seats row col)))
	      (cond
		((and (eq state :empty) (= 0 noccupied))
		 (progn
		   (setf (aref ret row col) :occupied)
		   (setf modifiedp t)))
		((and (eq state :occupied) (>= noccupied 4))
		 (progn
		   (setf (aref ret row col) :empty)
		   (setf modifiedp t))))))
	finally (return (values ret modifiedp))))

(defun total-occupied-seats (seats)
  (loop for i below (array-total-size seats)
	count (eq (row-major-aref seats i) :occupied)))

(defun simulate-occupied-seats ()
  (let ((seats (load-data)))
    (loop for n from 1 do
      (multiple-value-bind (new-seats modifiedp)
	  (update-seats seats)
	(unless modifiedp
	  (return (total-occupied-seats new-seats)))
	(format t "~a~%" n)
	(setf seats new-seats)))))
