(defpackage #:aoc2020/10
  (:use #:cl #:simple-graph)
  (:export
   #:find-adapter-chain
   #:count-arrangements))

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

;; ----------------------------------------------------------------------------

(defun build-count (graph target-node)
  (let ((count (make-hash-table)))
    ;; Populate count with zeros except for the target node
    (loop for n in (sg:get-nodes graph) do
      (setf (gethash n count) 0))
    (setf (gethash target-node count) 1)

    (loop with leaves = (sg:leaves graph)
	  while (/= 1 (length (sg:get-nodes graph)))
	  do
	     ;;(break)
	     (loop for leaf in leaves do
	       ;; add itself to each of its incoming nodes
	       (loop for in-node in (sg:get-in-nodes graph leaf) do
		 (incf (gethash in-node count) (gethash leaf count)))
	       ;; delete the leaf
	       (sg:remove-node graph leaf)
	       ;;(format t "~a~%" (length (sg:get-nodes graph)))
		   )
	  (setf leaves (sg:leaves graph)))
    count))

(defun count-arrangements ()
  ;; return count of first node
  (let* ((numbers (load-data))
	 (initial 0)
	 (final (+ 3 (apply #'max numbers)))
	 (graph (sg:make-graph-from-predicate (append (list initial final) numbers)
					      (lambda (x y) (member (- y x) '(1 2 3)))))
	 (count (build-count graph final))
	 )
    (gethash initial count)))


