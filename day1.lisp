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
      (if (gethash (- 2020 x) hash)
	  (return (* x (- 2020 x)))))))
