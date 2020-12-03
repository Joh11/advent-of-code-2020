(defpackage #:aoc2020/1
  (:use #:cl)
  (:export
   #:find-product-entries
   #:find-three-entries))

(in-package #:aoc2020/1)

(defun load-data (&optional (path "data/day1.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-integer line))))

(defun find-product-entries ()
  (let ((entries (load-data))
	(hash (make-hash-table)))
    ;; Fill the hash table
    (loop for x in entries do
      (setf (gethash x hash) x))
    ;; Find the pair
    (loop for x being the hash-key of hash do
      (when (gethash (- 2020 x) hash)
	  (return (* x (- 2020 x)))))))

(defun find-three-entries ()
  (let ((entries (load-data))
	(hash (make-hash-table)))
    ;; Fill the hash table
    (loop for x in entries do
      (setf (gethash x hash) x))
    ;; Find the pair
    (loop for x being the hash-key of hash do
      (loop for y being the hash-key of hash
	    ;; when (<= (+ x y) 2020)
	    do
	       (when (gethash (- 2020 x y) hash)
		 (return-from find-three-entries (* x y (- 2020 x y))))))))
