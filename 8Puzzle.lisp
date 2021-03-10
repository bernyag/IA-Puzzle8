;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions to simplify testing ;;;;;;;;;;;;;;;;;;;;;;

(setq goal '(0 1 2 3 4 5 6 7 8))
(setq test1 '(1 3 5 7 8 6 4 2 0)) ; estado solvible: costo 1715
(setq test2 '(0 1 2 3 4 8 5 7 6)) ; estado solvible: 286
(setq test3 '(1 0 2 3 4 6 5 7 8)) ; estado insolvible
(setq test4 '(1 2 3 4 0 5 6 7 8)) ; estado solvible: costo 282
(setq test5 '(3 1 2 4 7 5 6 0 8)) ; estado solvible: costo 10
(setq test6 '(3 1 2 4 7 5 0 6 8)) ; estado solvible: costo 12

;CORRER CON: (solver2 test5 'manhattan-total-cost)
 
(defun solver2 (state heuristic)
  (general-search state #'get-next-positions heuristic #'is-goal :samep #'equal :enqueue #'enqueue-priority :key #'priority-queue-key))

; Manhattan distance Heuristic
; My representation has the nice property (from the fact that index in my representation 
; where the tile should be IS the tile number itself. Ex. tile 7 at index 7) that:
; | index/3 - tilename/3 | = number of vertical moves to move into right position
; | index%3 - tilename%3 | = number of horizontal moves to move into right position
; That is, the sum of these IS the manhattan distance.
; The floor function returns both division and modulus in one fell swoop
; My "origin" here is 0,0 or the top,leftmost tile position

(defun manhattan-total-cost (current) 
  (if (not (null current) ) 
      (+ (manhattan-total-cost (cdr current ) ) (getCost  (- 9 (length current) ) (car current) ) )  
      0
  )
)

; Asigna a q y a r la división entera y el módulo, respectivamente, de index (la posición actual del número) entre 3
; Asigna a q2 y a r2 la división entera y el módulo, respectivamente, de goalIndex (a donde lo queremos mover) entre 3
; Hace la resta de q - q2 para calcular los movimientos en vertical
; Hace la resta de r - r2 para calcular los movimientos en horizontal
; La suma de estos dos da el costo
(defun getCost (index goalIndex)
    (+ 
      (abs (- (multiple-value-bind (q r) (floor index 3) q) (multiple-value-bind (q2 r2) (floor goalIndex 3) q2)))  
      (abs (- (multiple-value-bind (q r) (floor index 3) r) (multiple-value-bind (q2 r2) (floor goalIndex 3) r2)))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem Representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definition of 8 puzzle problem - List of all possible locations of blank (0)
; That is the problem representation is: 
; Given (a b c d e 0 f g h), where 0 is the blank and a b c d e f g h are any of the other tiles, 
; this corresponds to 5, for the location of the blank tile. It is not relevant what the 
; other tiles are, nor is representing the problem in the same way as a state.

; As for states, the state representation is a flat list with the locations of each piece on the board
; at that index in the list. The indices of the list are mapped to the board as follows:
; 0  1  2
; 3  4  5
; 6  7  8
; Next states are computed by the get-next-positions functions. Costs are not in the 
; problem definition as they are all unit. Heuristic values will be 
; determined when a node is created.
; I have also chosen to use 0 to represent the blank (not to be confused with
; position 0 on the board.

; A list of all possible locations of the blank piece
; I could have used a more complex representation such as
; '(0 a b c d e f g h)
; '(a 0 b c d e f g h)
; and so on but since we don't care about what else in the other slots, it serves
; to store only the blank location.

(defun zero-pos (current)
  (position 0 current)
)

; Estado deseado
(defun is-goal (state)
  (equal state '(0 1 2 3 4 5 6 7 8))
)

;regresa los posibles estados a los que se puede mover dado un estado
(defun get-next-positions (current)
  (let* ((blank (zero-pos current))
	(u (list 'arriba (up current blank)))
	(d (list 'abajo (down current blank)))
	(l (list 'izquierda (left current blank)))
	(r (list 'derecha (right current blank))))
    (filter (list u d l r) current)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers for get-next-positions function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (moves current)
  (cond
    ((null moves) nil)
    ((equal (cadar moves) current) (filter (cdr moves) current))
    (t (cons (car moves) (filter (cdr moves) current)))
  )
)

(defun up (current pos)
  (let ((aux (copy-list current)))
    (if (and (> pos 2) (<= pos 8))
      (rotatef (elt aux pos) (elt aux (- pos 3)))
    )
    aux
  )
)


(defun down (current pos)
  (let ((aux (copy-list current)))
    (if (and (<= pos 5))
      (rotatef (elt aux pos) (elt aux (+ pos 3)))
      
    )aux
  )
)

(defun right (current pos)
  (let ((aux (copy-list current)))
    (if (and (not (or (= pos 2) (= pos 5) (= pos 8))) (<= pos 8))
      (rotatef (elt aux pos) (elt aux (+ pos 1)))
      
    )aux
  )
)

(defun left (current pos)
  (let ((aux (copy-list current)))
    (if (and (not (or (= (position 0 current) 0) (= (position 0 current) 3) (= (position 0 current) 6))) (<= (position 0 current) 8))
      (rotatef (elt aux pos) (elt aux (- pos 1)))
      
    )aux
  )
)

;(nodeId, padre, profundidad, costo, dir, (x0, x1, x2, ... , x8))
(defvar *open* nil)
(defvar *closed* nil)
(defvar goal (list 0 1 2 3 4 5 6 7 8))
(defvar *counter* 1)

(defun init ()
  ;(setf nodo0 (list 1 0 0 0 NIL (list 8 6 7 2 5 4 3 0 1))) ;dificil
  (setf nodo0 (list 1 0 0 0 NIL (list 3 1 2 4 7 5 0 6 8))) ;facil
  (setf *open* nil)
  (setf *closed* nil)
  (setf *backtrack* nil)
  (setf *backtracker* nil)
  (setf *counter* 1)
  (setf (fourth nodo0) (manhattan-total-cost (get-current nodo0)))
  (setq flag 0)
  (push nodo0 *closed*)
)

(defun get-current (node)
  (car (last node))
)
(defun is-true (a)
	(eql a t))

;(nodeId, padre, profundidad, costo, dir, (x0, x1, x2, ... , x8))
(defun expand (node)
  (if (eq flag 0)
    (setf positions (get-next-positions (get-current node)))
  )
  (setf child (copy-list node))
  (if (not (null positions) ) 
    (progn 
      (incf *counter*)
      (setf (first child) *counter*)
      (setf (second child) (first node))
      (setf (third child) (incf (third child)))
      (setf (fifth child) (caar positions))
      (setf (sixth child) (cadar positions))
      (setf (fourth child) (+ (manhattan-total-cost (get-current child)) (fourth node) ))
      (setf positions (cdr positions))
       (cond
          ((some #'is-true (mapcar #'(lambda (x) (equal (sixth child) (sixth x))) *closed* )))
          (t 
            (cond
              ((some #'is-true (mapcar #'(lambda (x) (equal (sixth child) (sixth x))) *open* )))
              (t (push child *open*)
              ))))    
      (incf flag)
      (expand node)
)) (setq flag 0))

(defun solver ()
  (init)
  (expand nodo0)
  (dotimes (n 1000000)
    (sort-list)
    ; popear los 2 de hasta arriba y mandarlos a expand()
    (if (> (length *open*) 200) (setf *open* (subseq *open* 0 100)) 0)
    (setf next (car *open*))
    ;(if (is-goal (sixth next)) (return (backtrack (car *open*))) 0)
    (push (pop *open*) *closed*)
    (if (is-goal (sixth next)) (return t) 0)
    (expand next)
  )
)

(defun backtrack (father-id list)
  (setf closed-list (copy-list list))
  (if (not (null closed-list) )
    (if (= (caar closed-list) father-id)
      (progn
        (push (car closed-list) *backtrack* )
        (push (fifth (car closed-list)) *backtracker* )
        (backtrack (cadar closed-list) (cdr closed-list))
      )
      (backtrack father-id (cdr closed-list))
    )
  )t 
)

(defun compare-depth (a b)
  (> (third a) (third b)))

(defun compare-cost (a b)
  (< (fourth a) (fourth b))
)

(defun sort-list ()
  (stable-sort 
    (sort *open* 'compare-cost) 'compare-depth)
)