(defun printGrid(n l)

  (format t "~a~%" n)
  (format t "~a~%" l)
  (loop for x in l
    do 
    (loop for i from 1 to n
      do
      ;(format t "~A " x) 
      ;;; Print the spacing in the grid
      (if (equal i x) (format t "q ") (format t ". "))
      (if (equal 0 (mod i n)) (format t "~%"))
    )
  )
  (format t "~%")
)


;;; If the diagonal is blocked return false else return true
;;; Takes the current row and column and the current solution state
(defun isDiag (n row col sol)
  (setq return T)
  (setq r row)
  (setq c col)

  (loop while (and (> r 1) (>= c 1) )
    do
    (setf r (- r 1))
    (setf c (- c 1))
    (if (and (<= r (list-length sol)) (equal c (nth (- r 1) sol))) (setf return nil) ())
  )

  (setf r row)
  (setf c col)

  (loop while (and (> r 1) (<= c n) )
    do
    (setf r (- r 1))
    (setf c (+ c 1))
    (if (and (<= r (list-length sol)) (equal c (nth (- r 1) sol))) (setf return nil) ())
  )

  (setf return return)
)


;;; If the column is blocked return false else return true
;;; Takes a column and the current solution state
(defun isCol (col sol)
  (setq return T)

  (loop for k in sol
    do
    (if (equal k col) (setf return nil) () )
  )
  (setf return return)
)

;;; If the row is blocked return false else return true
;;; Takes the current column and the current solution state
(defun isRow (row sol)
  (setq return T)
  (setq i 1)

  (loop for k in sol
    do
    (if (equal i row) (setf return nil) () )
    (setf i (+ i 1))
  )
  (setf return return)
)




;;;State will be repersented by a list of positions 
;;; the first element in the list is the position in the first row
;;; the second element on the list the senond row and so forth. 
;;;

(defun FindQueens() 
  
  (setq sol '())
  (setq row 0)
  (setq col 0)
  (setq failState nil)
  (setq failState2 nil)
  


  ;;; Loop through finding all of the solutions from 4 to 1000
  (loop for i from 4 to 100
    do 

    ;;; Loops for iterating through the states
    ;;; the first loop is rows (j) and the second loop is columns (k)
    (loop for j from 1 to i
      do
      (loop for k from 1 to i 
        do 

        ;;; If the diagonal is not blocked AND the row is not blocked AND the column is not blocked then
        ;;; this is a potential state so add it to the list
        (if (and (isDiag i j k sol) (isCol k sol) (isRow j sol)) 
            (if (equal (list-length sol) 0) 
                (setf sol (cons k sol)) 
                (setf sol (append sol (list k)))
            ) 
            ()
        )


        ;;; If we have reached the end of the grid and have not placed all of the queens start to ack track and try differeent states.
        ;;; Or if we reach the end of a row with no queens placed.
        ;;; And the solution has not been found (only need to find one solution).
        (setf failState (or (and (equal i k) (< (list-length sol) j) ) (and (equal i j) (equal i k))))
        
        ;;; If the solution is found this is not a failstate 
        (if (equal i (list-length sol)) (setf failState nil) () )

        (if failState 
            (progn 
              (setf j (list-length sol))
              (setf k (nth (- (list-length sol) 1) sol))

              ) ())
        
        (if failState (setf sol (reverse (cdr (reverse sol)))) ())

        ;;; If the state that showed up after being poped off is in the nth col pop another off.
        ;;; This only works because you cannot have a state in the solution that has two queens on the same column.
        (setf failState2 (and (equal i k) failstate))

        (if failState2 
            (progn 
              (setf j (list-length sol))
              (setf k (nth (- (list-length sol) 1) sol))

              ) ())
        
        (if failState2 (setf sol (reverse (cdr (reverse sol)))) ())
        
        
        
      )
    )  
    ;;; Print the solution found for the nth iteration
    (printGrid i sol)
  

 
  )
)





(findQueens)







