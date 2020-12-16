(defpackage #:aoc2020/12
  (:use #:cl)
  (:export
   ))

(in-package #:aoc2020/12)

(defun parse-instruction (line)
  (let* ((c (aref line 0))
	 (instr (cond
		  ((char= c #\N) :north)
		  ((char= c #\S) :south)
		  ((char= c #\E) :east)
		  ((char= c #\W) :west)
		  ((char= c #\L) :left)
		  ((char= c #\R) :right)
		  ((char= c #\F) :forward))))
    (list instr (parse-integer (subseq line 1)))))

(defun load-data (&optional (path "data/day12.dat"))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-instruction line))))

(defun make-state (x y direction)
  (list x y direction))

(defun turn-left (direction)
  (case direction
    (:north :west)
    (:south :east)
    (:east :north)
    (:west :south)))

(defun turn-right (direction)
  (case direction
    (:north :east)
    (:south :west)
    (:east :south)
    (:west :north)))

(defun apply-n (f n x)
  (loop for i from 1 to n do
    (setf x (funcall f x)))
  x)

(defun move-forward (state n)
  (let ((x (first state))
	(y (second state))
	(direction (third state)))
    (case direction
      (:north (list x (+ y n) direction))
      (:south (list x (- y n) direction))
      (:east (list (+ x n) y direction))
      (:west (list (- x n) y direction)))))

(defun manhattan-dist (state &optional state-b)
  (unless state-b
    (setf state-b '(0 0)))
  (+ (abs (- (first state) (first state-b)))
     (abs (- (second state) (second state-b)))))

(defun move (state instruction)
  (let ((x (first state))
	(y (second state))
	(direction (third state))
	(i (first instruction))
	(n (second instruction)))
    (case i
      (:north (list x (+ y n) direction))
      (:south (list x (- y n) direction))
      (:east (list (+ x n) y direction))
      (:west (list (- x n) y direction))
      (:left (list x y (apply-n #'turn-left (/ n 90) direction)))
      (:right (list x y (apply-n #'turn-right (/ n 90) direction)))
      (:forward (move-forward state n)))))

(defun compute-final-position ()
  (let ((instrs (load-data))
	(state (make-state 0 0 :east)))
    (loop for i in instrs do
      (setf state (move state i))
	  finally (return (manhattan-dist state)))))

;; ----------------------------------------------------------------------------

(defun turn-left-v2 (n x-waypoint y-waypoint)
  (loop while (/= n 0) do
    (decf n 90)
    (psetf x-waypoint (* -1 y-waypoint)
	   y-waypoint x-waypoint))
  (values x-waypoint y-waypoint))

(defun turn-right-v2 (n x-waypoint y-waypoint)
  (loop while (/= n 0) do
    (decf n 90)
    (psetf x-waypoint y-waypoint
	   y-waypoint (* -1 x-waypoint)))
  (values x-waypoint y-waypoint))

(defun move-forward-v2 (state n)
  (loop for i below n do
    (incf (first state) (third state))
    (incf (second state) (fourth state)))
  (values (first state) (second state)))

(defun move-v2 (state instruction)
  (let ((x (first state))
	(y (second state))
	(x-waypoint (third state))
	(y-waypoint (fourth state))
	(i (first instruction))
	(n (second instruction)))
    (case i
      (:north (incf y-waypoint n))
      (:south (decf y-waypoint n))
      (:east (incf x-waypoint n))
      (:west (decf x-waypoint n))
      (:left (multiple-value-setq (x-waypoint y-waypoint)
	       (turn-left-v2 n x-waypoint y-waypoint)))
      (:right (multiple-value-setq (x-waypoint y-waypoint)
	       (turn-right-v2 n x-waypoint y-waypoint)))
      (:forward (multiple-value-setq (x y)
		  (move-forward-v2 state n))))
    (make-waypoint-state x y x-waypoint y-waypoint)))

(defun make-waypoint-state (x y x-waypoint y-waypoint)
  (list x y x-waypoint y-waypoint))

(defun compute-final-position-with-waypoint ()
  (let ((instrs (load-data))
	(state (make-waypoint-state 0 0 10 1)))
    (loop for i in instrs do
      (setf state (move-v2 state i))
	  finally (return (manhattan-dist state)))))
