;;;; advent-of-code-2020.asd

(asdf:defsystem #:advent-of-code-2020
  :description "Advent of Code 2020"
  :author "Johan Felisaz"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("anaphora" "cl-ppcre")
  :components ((:file "package")
               (:file "advent-of-code-2020")
	       (:file "day1")))
