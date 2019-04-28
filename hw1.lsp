;PA1 Venus Sun

;Q1 Generate
;the logic here is to generate all permutations of lists for number 2 to N
;and then select N+1 of them and append 1 to the beginning.

(defun generate (x)
  (mapcar #'(lambda (l) (cons 1 l))
  (pick-perm (+ 1 x) (all-perm (cddr (loop for n below (+ 1 x) collect n))) '())))

;this function below generates all possible permutations of a list
(defun all-perm (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-perm (remove element list)))))))

;this function below picks a certain amount of items in a list randomly
(defun pick-perm (x perms result)
  (cond ((< x 1) result)
	(t (pick-perm (- x 1) perms (cons (nth (random (list-length perms)) perms) result)))))

;Q2 Solve
;as for a brute force solution, here I basically permutate the location of all the nuts on the board
;and then align the edge ones to see if the board is finished
(defun solve (board)
  (loop for seq in (all-perm (loop for n below (list-length board) collect n))
	for trial = (align-center (arrange board seq))
	when (eq T (done-state trial 1 (list-length (car board)) ))
	return trial))

;this below arranges the order of the nut using a certain permuation of sequence
(defun arrange (board seq)
  (loop for num in seq
	collect (nth num board)))

;this is a helper method to check if the board is finished
(defun done-state (board i a)
  (cond ((eq i a)
	 (eq (get-num board i (- i 1) a) (get-num board (+ i 1) (+ i 2) a)))
	(t (and (eq (get-num board i (- i 1) a) (get-num board  (+ i 1) (+ i 2) a)) (done-state board (+ i 1) a)))))

;the function below is a helper method to access numbers in the nut easier, for used in the "done-state" checker
(defun get-num (board i j a)
  (cond ((< j 1) (get-num board i (+ j a) a))
	((and (> j a) (<= i a))  (get-num board i (- j a) a))
	((and (> j a) (> i a))  (get-num board (- i a) (- j a) a))
	(t (nth (- j 1) (nth i board)))))


;this below aligns each of the edge nuts to the center nut
(defun align-center (board)
  (cons (car board)
	  (loop for i from 1 to (- (list-length board) 1)
		collect (position-one i (car board) (nth i board)))))

;this below aligns one of the edge nut to the center nut
(defun position-one (i center toposition)
  (if (eq (nth (- i 1) toposition) (nth (- i 1) center))
      toposition
    (position-one i center (rotate toposition))))

;this below rotates one nut clockwisely
(defun rotate (list)
  (append (cdr list) (list (car list))))

	
;Q3 Pruning
;the strategy here is to first put down two nuts and then using dfs to start putting down nuts one by one and
;"prune" as soon as it's no longer possible to complete the puzzle.

;this function bascially loops through the choices for the first two to put down and then call dfs upon
(defun solve-dfs (board)
  (loop for nut in board
	thereis (loop for restnut in (remove nut board)
		      thereis (dfs (list nut (position-one 1 nut restnut)) (remove restnut (remove nut board)) (list-length board)))))

;this is the dfs where it only continues putting down nuts as each nut placed is in the "correct" state
(defun dfs (down left size)
  (cond	((and (eq 0 (list-length left)) (eq size (list-length down)) (check-last down size)) T)
	((or (< (list-length down) 3) (check-last down size))
	 (loop for nut in left
	       thereis (dfs (put down nut) (remove nut left) size)))
	 ))

;this is a helper method to help put down nuts aligning with center
(defun put (down nut)
  (append down (list (position-one (list-length down) (car down) nut)))
  )

;this is a helper method to help check if the last placed nut is matched with the previous on the edge they touch
(defun check-last (down size)
  (eq (get-num down (- (list-length down) 1) (list-length down) (- size 1))
      (get-num down (- (list-length down) 2) (- (list-length down) 3) (- size 1)))
   )

;And of course, dfs performs better than brute-force for bigger Ns.

;Q4 Count

;this below collects all the results from running the dfs
(defun result-dfs (board)
  (loop for nut in board
	append (loop for restnut in (remove nut board)
		 collect (dfs (list nut (position-one 1 nut restnut)) (remove restnut (remove nut board)) (list-length board))
		 )))
;this below counts all the solutions found from runnign the dfs
(defun count-dfs (board)
  (loop for item in (result-dfs board)
	count (eq T item)
	))

;Q5 Graph
;this below generates a sized N puzzle for a defined-number of times and then solve them to return how many solutions there are
(defun solve-puzzle (n times)
  (loop for a from 1 to times
	collect (count-dfs (generate n))))

;this counts how many puzzles have 0 result, how many have 1 result, and how many have more than 1 results
(defun count-puzzle (result)
  (list (count 0 result)
	(count 1 result)
	(- (list-length result) (+ (count 0 result) (count 1 result)))
	)
  )

;Q6 Main
;this solves the standard hexnut puzzle
(defun main()
  (solve '((1 6 5 4 3 2) (1 6 4 2 5 3) (1 2 3 4 5 6) (1 6 2 4 5 3) (1 4 3 6 5 2) (1 4 6 2 3 5) (1 6 5 3 2 4)))
)
