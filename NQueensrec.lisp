
;;; BVariable to count the number of solutions for each n
(defvar solnum 0)


(defun printGrid(n l)

  (format t "~a~%" n)
  (format t "~a~%" l)
  (loop for x in l
    do 
    (loop for i from 0 to (- n 1)
      do
      ;(format t "~A " x) 
      ;;; Print the spacing in the grid
      (if (equal i x) (format t "q ") (format t ". "))
      (if (equal (- n 1) (mod i n)) (format t "~%"))
    )
  )
  (format t "~%")
)

(defun printCoord(n l)
  (setq i 0)
  (format t "(")
  (loop for x in l
    do
    (format t "(~A, ~A) " i x)
    (setf i (+ i 1))
  )
  (format t ")~%~%")
)


;;; If the diagonal is blocked return false else return true
;;; Takes the current row and column and the current solution state
(defun isDiag (n row col sol)
  (setq return T)
  (setf return (and (isDiagRight n row col sol) (isDiagLeft n row col sol)))
)

;;; Function to check if the diagonal to the upper right has any conflicts.
;;; 
(defun isDiagRight (n row col sol) 
  (setq return T)
  ;;; If the next space on the diagonal is in the grid. 
  (if (and (> row 0) (< col n)) (isDiagRight n (- row 1) (+ col 1) sol) ())
  (if (and (<= row (list-length sol)) (equal col (nth row sol))) (setf return nil) ())
  (setf return return)

)

;;; Function to check if diagonal to the upper left has any conflicts. 
(defun isDiagLeft (n row col sol) 
  (setq return T)
  (if (and (> row 0) (>= col 0)) (isDiagLeft n (- row 1) (- col 1) sol) ())
  (if (and (<= row (list-length sol)) (equal col (nth row sol))) (setf return nil) ())
  (setf return return)
)



;;; If the column is blocked return false else return true
;;; Takes a column and the current solution state
;;; TODO make this rec
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

  (if (< row (list-length sol)) (setf return nil) () )

  (setf return return)
)




(defun FindQueens() 
  (loop for n from 1 to 100
    do 
    (recQueens n 0 (* n n) '())
    (format t "The number of solutions for ~A queens is: ~A~%" n solnum)
    (setf solnum 0)
  )


)


(defun recQueens (n curr len sol) 

  (setq row (floor (/ curr n)))
  (setq col (mod curr n))
  (setq valid nil)
  (setq done nil)
  (setq pop nil)
  (setq found nil)

  (setf valid (and (isDiag n row col sol) (isRow row sol) (isCol col sol) ))
  
  (if (and valid (not (equal nil sol))) (setf sol (reverse(cons col (reverse sol)))) ())
  (if (and valid (equal nil sol)) (setf sol (cons col sol)) ())

  ;;; If there are still states left and we have not skiped a row call the next instance.
  ;;; And we have queens laft to place.
  (if (and (not (equal n (list-length sol))) (< curr len) (not (and (equal col (- n 1)) (equal row (list-length sol)))) ) 
      (setf sol (recQueens n (+ curr 1) len sol)) ())




  ;;; Check if this is the solution.
  ;;; If it is print it out.
  (setf found (equal n (list-length sol)))
  ;(if found (printGrid n sol) ())
  ;(if found (printcoord n sol) ())
  (if found (setf solnum (+ solnum 1)) ()) 

  ;;; If we have reach the last slot in n.
  ;(if (equal curr len) (setf done T) ())
  
  ;;; Pop a state off and head back.
  ;;; While the final state in the sol list is not equal to the current state.
  (setf row (floor (/ curr n)))
  (setf col (mod curr n))

  ;;; pop if this is the corrrect element to remove
  (setf pop (and (equal row (- (list-length sol) 1)) (equal col (nth (- (list-length sol) 1) sol))))

  (if pop (setf sol (reverse (cdr (reverse sol)))) ())
 
  (if (and pop (not (equal n (list-length sol))) (not (equal col (- n 1)))) (setf sol (recQueens n (+ curr 1) len sol)) ())

  ;;; Return stuff
  (setf sol sol)

  
)



(findQueens)



