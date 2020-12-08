(defpackage #:aoc2020/7
  (:use #:cl)
  (:export
   #:count-can-contain
   #:count-inside-bags))

(in-package #:aoc2020/7)

(defun str-to-symbol (str)
  (intern (string-upcase (ppcre:regex-replace-all " " str "-"))))

(defun parse-line (line)
  (let* ((tokens (ppcre:split " bags contain " line))
	 (color (str-to-symbol (car tokens)))
	 (other-bags (second tokens)))
    (if (string= "no other bags." other-bags)
	(values color nil)
	(values color
		(loop for x in (ppcre:split ", " other-bags)
		      appending (multiple-value-bind (str splits) (ppcre:scan-to-strings "([0-9]*) (.*)" (ppcre:regex-replace-all " (bags|bag)\\.?" x ""))
				  (list (str-to-symbol (elt splits 1))
					(parse-integer (elt splits 0)))))))))

(defun load-data (&optional (path "data/day7.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  with hash = (make-hash-table)
	  do (multiple-value-bind (color contains) (parse-line line)
	       (setf (gethash color hash) contains))
	  finally (return hash))))

(defun count-can-contain (&optional (color 'shiny-gold))
  (let ((rev-hash (make-hash-table))
	(hash (load-data)))
    ;; Build reverse hash
    (loop for c being the hash-keys of hash
	  do (loop for cc in (gethash c hash)
		   for i from 0 when (evenp i)
		   do (push c (gethash cc rev-hash))))
    (loop with old-count = 0
	  with hash = (let ((x (make-hash-table))) (setf (gethash color x) t) x)
	  while (/= old-count (hash-table-count hash)) do
	    (setf old-count (hash-table-count hash))
	    (loop for c being the hash-key of hash do
	      (loop for cc in (gethash c rev-hash) do
		(setf (gethash cc hash) t)))
	  finally (return (1- (hash-table-count hash))))))

(defun count-inside-bags (&optional (color 'shiny-gold))
  (let ((hash (load-data)))
    (labels ((rec (bags)
	       (1+ (loop for (color num) on bags while num
			 for i from 0 when (evenp i)
			 sum (* num (rec (gethash color hash)))))))
      (1- (rec (gethash color hash))))))
