(defpackage #:aoc2020/4
  (:use #:cl)
  (:export
   #:count-valid-passports))

(in-package #:aoc2020/4)

(defun add-kv-pairs (line hash)
  (ppcre:do-register-groups (k v)
      ("(\\w+):(\\S+)" line)
    (setf (gethash k hash) v)))

(defun load-data (&optional (path "data/day4.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil) while line
	  with passports = (list (make-hash-table :test 'equal)) do
	    (cond
	      ((string= line "") (push (make-hash-table :test 'equal) passports))
	      (t (add-kv-pairs line (car passports))))
	  finally (return passports))))

(defun validp (passport)
  (let ((required-keys (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (every (lambda (key) (gethash key passport))
	   required-keys)))

(defun count-valid-passports (&optional (validation-fun #'validp))
  (loop for passport in (load-data)
	count (funcall validation-fun passport)))

(defun four-digits-inp (str min max)
  (and (ppcre:scan "^[0-9]{4}$" str)
	    (<= min (parse-integer str) max)))

(defun heightp (str)
  (multiple-value-bind (res reg) (ppcre:scan-to-strings "^([0-9]+)(cm|in)$" str)
    (and res
	 (if (string= (aref reg 1) "cm")
	     (<= 150 (parse-integer (aref reg 0)) 193)
	     (<= 59 (parse-integer (aref reg 0)) 76)))))

(defun valid-more-strictp (passport)
  (when (validp passport) ;; Pass the key existence test first
    (let ((byr (gethash "byr" passport))
	  (iyr (gethash "iyr" passport))
	  (eyr (gethash "eyr" passport))
	  (hgt (gethash "hgt" passport))
	  (hcl (gethash "hcl" passport))
	  (ecl (gethash "ecl" passport))
	  (pid (gethash "pid" passport)))
      (and
       (four-digits-inp byr 1920 2002)
       (four-digits-inp iyr 2010 2020)
       (four-digits-inp eyr 2020 2030)
       (heightp hgt)
       (ppcre:scan "^#[0-9a-f]{6}$" hcl)
       (ppcre:scan "^(amb|blu|brn|gry|grn|hzl|oth)$" ecl)
       (ppcre:scan "^[0-9]{9}$" pid)))))
