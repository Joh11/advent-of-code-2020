;;;; advent-of-code-2020.asd

(asdf:defsystem #:advent-of-code-2020
  :description "Advent of Code 2020"
  :author "Johan Felisaz"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "day1")
	       (:file "day2")
	       (:file "day3")))
