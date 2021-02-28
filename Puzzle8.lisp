(setq goal '( 1 2 3 4 5 6 7 8 0))

(defun up (current zeroPos)
  (let ((aux (copy-list current)))
    (if (and (> zeroPos 2) (< zeroPos 8))
        (rotatef (elt aux zeroPos) (elt aux (- zeroPos 3)))
    )
    aux
  )
)

(defun down (current zeroPos)
  (let ((aux (copy-list current)))
    (if (and (< zeroPos 5) (< zeroPos 8))
        (rotatef (elt aux zeroPos) (elt aux (+ zeroPos 3)))
    )
    aux
  )
)

(defun right (current zeroPos)
  (let ((aux (copy-list current)))
    (if (and (not (or (= zeroPos 2) (= zeroPos 5) (= zeroPos 8))) (< zeroPos 8))
        (rotatef (elt aux zeroPos) (elt aux (+ zeroPos 1)))
    )
    aux
  )
)

(defun left (current zeroPos)
  (let ((aux (copy-list current)))
    (if (and (not (or (= zeroPos 0) (= zeroPos 3) (= zeroPos 6))) (< zeroPos 8))
        (rotatef (elt aux zeroPos) (elt aux (- zeroPos 1)))
    )
    aux
  )
)

(setq varTotalCost 0)
(defun getTotalCost (current goal)
    (cond 
        ((null current))
        (t (incf varCost (car lst)) (sum (cdr lst)))
    )
)

; Asigna a q y a r la división entera y el módulo, respectivamente, de index (la posición actual del número) entre 3
; Asigna a q2 y a r2 la división entera y el módulo, respectivamente, de goalIndex (a donde lo queremos mover)entre 3
; Hace la resta de q - q2 para calcular los movimientos en vertical
; Hace la resta de r - r2 para calcular los movimientos en horizontal
; La suma de estos dos da el costo
(setq varCost 0)
(defun getCost (index goalIndex)
    (+ (multiple-value-bind (q r) (floor index 3) (+ q r)) (abs (multiple-value-bind (q2 r2) (floor goalIndex 3) (- q2 r2))))
)