(defpackage #:aoc2020/13
  (:use #:cl #:alexandria)
  (:export
   ))

(in-package #:aoc2020/13)

(defun parse-bus-ids (line)
  (mapcar #'parse-integer (remove "x" (ppcre:split "," line) :test #'string=)))

(defun load-data (&optional (path "data/day13.dat"))
  (with-open-file (stream path)
    (let ((ready-at (parse-integer (read-line stream)))
	  (bus-ids (parse-bus-ids (read-line stream))))
      (values ready-at bus-ids))))

(defun earliest-bus (ready-at bus-ids)
  (let* ((waiting-times (mapcar (lambda (n)
				  (list (- n (rem ready-at n)) n))
				bus-ids))
	 (best-one (reduce (lambda (a b)
			     (if (< (first a) (first b))
				 a
				 b))
			   waiting-times)))
    (values (first best-one)
	    (second best-one))))

(defun part1 ()
  (multiple-value-bind (id time)
      (apply #'earliest-bus (multiple-value-list (load-data)))
    (* id time)))

;; ----------------------------------------------------------------------------

(defun parse-bus-ids-part2 (line)
  (loop for s in (ppcre:split "," line)
	for k from 0
	unless (string= s "x")
	  collect (parse-integer s) into bus-ids and
	collect k into shifts
	finally (return (values bus-ids shifts))))

(defun load-data-part2 (&optional (path "data/day13.dat"))
  (with-open-file (stream path)
    (read-line stream)
    (parse-bus-ids-part2 (read-line stream))))

(defun dividesp (a b)
  "Returns true if a | b, that is there exists k such that b = ka. "
  (= 0 (rem b a)))

(defun extended-euclid (a b)
  "Returns a list (x y) such that ax+by=1"
  ;; Edge cases
  (when (and (< a 0) (< b 0))
    (return-from extended-euclid (mapcar (lambda (x) (- x))
					 (extended-euclid (- a) (- b)))))
  (let ((r (list b a))
	(u (list 0 1))
	(v (list 1 0)))
    (loop until (= 1 (abs (car r))) do
      (multiple-value-bind (q k) (floor (second r) (first r))
	(push k r)
	(push (- (second u) (* q (first u))) u)
	(push (- (second v) (* q (first v))) v)))
    (if (= 1 (car r))
	(list (car u) (car v))
	(list (- (car u)) (- (car v))))))

(defun simplify-diophantine-solution (x0 y0 xh yh)
  (if (< (abs x0) (abs y0))
      (destructuring-bind (y0 x0 yh xh)
	  (simplify-diophantine-solution y0 x0 yh xh)
	(list x0 y0 xh yh))
      (let ((quot (round x0 xh)))
	(list (- x0 (* quot xh))
	      (- y0 (* quot yh))
	      xh yh))))

(defun solve-diophantine-eq (a b c)
  "Find all solutions of the integer valued equation A*x + B*y =
  C. Returns a list of 4 numbers, x_p, y_p the particular solution,
  and x_h, y_h, the homogeneous solution. "
  (let ((g (gcd a b)))
    (unless (dividesp g c)
      (error "There is no solution !"))
    (let ((couple (extended-euclid (/ a g) (/ b g))))
      ;; to make sure the homogeneous solution is positive if possible
      (when (and (< b 0) (> a 0))
	(setf b (- b))
	(setf a (- a)))
      (simplify-diophantine-solution (* (first couple) (/ c g)) (* (second couple) (/ c g))
				     (/ b g) (- (/ a g))))))

(defun solve-diophantine-system (ns ks)
  (let ((nconst 0) ;; constant part of N
	(nmult 1) ;; multiplicative factor of N
	(max-id (apply #'max ns)))
    (loop while ns do
      (destructuring-bind (n0 alpha0 nh alphah)
	  (solve-diophantine-eq nmult (- (car ns)) (- (car ks) nconst))
	;; update the expression for N
	(format t "current: N = ~a + ~a * t~%" nconst nmult)
	(format t "must have N - ~a * ɑ = ~a~%" (car ns) (car ks))
	(format t "(solve-diophantine-eq ~a ~a ~a)~%" nmult (- (car ns)) (- (car ks) nconst))
	(incf nconst (* nmult n0))
	(setf nmult (* nmult nh))
	(format t "~a = ~a * ~a + ~a, should be ≡ ~a (~a)~%"
		nconst
		(round nconst (car ns))
		(car ns)
		(mod nconst (car ns)) (car ks) (car ns))
	;; pop the used coefficients
	(pop ns) (pop ks)
	;; To keep N reasonable
	;; (when (< (abs nmult) (abs nconst))
	;;   (setf nconst (floor nconst nmult)))
	))
    (setf nconst (abs nconst))
    ;; One need every bus to start first
    (loop while (< nconst max-id) do
	  (incf nconst nmult))
    (list nconst nmult)))

(defun check-solution (x)
  (multiple-value-bind (ns ks)
      (load-data-part2)
    (mapcar (lambda (n k)
	      (list k (rem x n)))
	    ns ks)))

(defun part2 ()
  (multiple-value-bind (ns ks) (load-data-part2)
    (let* ((n (length ns))
	   (mat (make-array `(,n (+ 2 n))
			    :initial-element 0)))
      ;; Fill the matrix
      (dotimes (i n)
	(setf (aref mat i 0) 1)
	(setf (aref mat i (1+ i)) (- (elt ns i)))
	(setf (aref mat i (1+ n)) (elt ks i)))
      (format t "~a~%" mat)
      ;; Reduce it
      (setf mat (row-echelon-form mat))
      (format t "~a~%" mat)
      ;; Add every row to the previous one for reduced row echelon
      ;; form
      (loop for i from (1- n) downto 1 do
	(dotimes (k (+ 2 n))
	  (setf (aref mat (1- i) k)
		(+ (aref mat (1- i) k) (aref mat i k)))))
      ;; Find the kernel basis vector
      (let ((k-vec (kernel-basis-vector mat))))
      (format t "~a~%" mat))))

(defun find-max-row (matrix column &optional (start-row 0))
  "Returns the index of the row with the biggest element in the given
  column (starting from the start-row). "
  (loop for i from (1+ start-row) below (array-dimension matrix 0)
	with max = (aref matrix start-row column) and argmax = start-row
	when (> (abs (aref matrix i column)) max) do
	  (setf max (aref matrix i column))
	  (setf argmax i)
	finally (return argmax)))

(defun swap-rows! (matrix i j)
  (let* ((n (array-dimension matrix 1))
	 (tmp (make-array n)))
    ;; Copy the ith row to the tmp
    (dotimes (k n)
      (setf (aref tmp k) (aref matrix i k)))
    ;; Copy jth to ith
    (dotimes (k n)
      (setf (aref matrix i k) (aref matrix j k)))
    ;; Copy tmp to jth
    (dotimes (k n)
      (setf (aref matrix j k) (aref tmp k)))
    matrix))

(defun row-echelon-form (matrix &key reducep)
  "Return the row echelon form of an integer valued matrix. If
REDUCEP, returns the reduced row echelon form. "
  (let ((ret (copy-array matrix))
	(dim (array-dimension matrix 0)))
    (dotimes (i (1- dim) ret)
      ;; Put the row with with largest first element in front
      (swap-rows! ret i (find-max-row ret i i))
      ;; For each row below
      (loop for j from (1+ i) below dim
	    when (/= 0 (aref ret j i))
	    do
	(let* ((m (lcm (aref ret i i) (aref ret j i)))
	       (a (/ m (aref ret j i)))
	       (b (/ m (aref ret i i))))
	  (dotimes (k (array-dimension ret 1))
	    (setf (aref ret j k) (- (* (aref ret j k) a)
				    (* (aref ret i k) b)))))))))

(defun find-matching-time (bus-ids shifts)
  (let ((pairs (mapcar #'list bus-ids shifts)))
    ;; Sort them by decreasing order (to decrease the number of iterations)
    (setf pairs (sort pairs #'> :key #'car))
    (loop for i from 0
	  do (format t "~a~%" (* i (first (car pairs))))
	  when (every (lambda (nk)
			(= (second nk) (rem (* i (first (car pairs))) (first nk))))
		      (cdr pairs))
	    return (* i (first (car pairs))))))
