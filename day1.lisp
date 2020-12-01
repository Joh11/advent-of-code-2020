(in-package #:advent-of-code-2020)

(defun load-data (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-integer line))))

(defun find-product-entries ()
  (let ((entries (load-data "data/day1.dat"))
	(hash (make-hash-table)))
    ;; Fill the hash table
    (loop for x in entries do
      (setf (gethash x hash) x))
    ;; Find the pair
    (loop for x being the hash-key of hash do
      (when (gethash (- 2020 x) hash)
	  (return (* x (- 2020 x)))))))

(defun find-three-entries ()
  (let ((entries (load-data "data/day1.dat"))
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
