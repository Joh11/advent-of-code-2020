(defpackage #:aoc2020/10
  (:use #:cl)
  (:export
   ))

(in-package #:aoc2020/10)

(defun load-data (&optional (path "data/day10.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-integer line))))

(defun valid-chainp (chain)
  (and (member (first chain) '(0 1 2 3))
       (every (lambda (x) (member x '(0 1 2 3)))
	      (mapcar #'- (cdr chain) chain))))

(defun find-adapter-chain ()
  (let ((chain (sort (load-data) #'<)))
    (unless (valid-chainp chain)
      (return-from find-adapter-chain))
    (let ((diffs (mapcar #'- chain (cons 0 chain))))
      (* (1+ (count 3 diffs)) (count 1 diffs)))))
