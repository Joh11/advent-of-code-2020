;;;; advent-of-code-2020.asd

(asdf:defsystem #:advent-of-code-2020
  :description "Advent of Code 2020"
  :author "Johan Felisaz"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "simple-graph")
  :components ((:file "day1")
	       (:file "day2")
	       (:file "day3")
	       (:file "day4")
	       (:file "day5")
	       (:file "day6")
	       (:file "day7")
	       (:file "day8")
	       (:file "day9")
	       (:file "day10")
	       (:file "day11")
	       (:file "day12")))
