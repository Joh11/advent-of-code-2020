;;;; advent-of-code-2020.asd

(asdf:defsystem #:advent-of-code-2020
  :description "Describe advent-of-code-2020 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("anaphora" "cl-ppcre")
  :components ((:file "package")
               (:file "advent-of-code-2020")
	       (:file "day1")))
