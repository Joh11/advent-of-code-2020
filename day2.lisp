(in-package #:advent-of-code-2020)

(defun parse-line (line)
  (let* ((xs (ppcre:split ":" line))
	 (password (subseq (second xs) 1))
	 (ys (ppcre:split "[- ]" (first xs)))
	 (min (parse-integer (first ys)))
	 (max (parse-integer (second ys)))
	 (char (elt (third ys) 0)))
    `(:min ,min :max ,max :char ,char :password ,password)))

(defun load-data-day2 (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-line line))))

(defun valid-passwordp (pw)
  (let ((nchar (count (getf pw :char) (getf pw :password))))
    (<= (getf pw :min) nchar (getf pw :max))))

(defun valid-password-v2p (pw)
  (= 1 (count (getf pw :char)
	      (list (elt (getf pw :password) (1- (getf pw :min)))
		    (elt (getf pw :password) (1- (getf pw :max)))))))

(defun count-valid-passwords ()
  (let ((passwords (load-data-day2 "data/day2.dat")))
    (loop for pw in passwords
	  count (valid-passwordp pw))))

(defun count-valid-passwords-v2 ()
  (let ((passwords (load-data-day2 "data/day2.dat")))
    (loop for pw in passwords
	  count (valid-password-v2p pw))))