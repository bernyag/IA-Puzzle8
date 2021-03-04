(setq goal '(0 1 2 3 4 5 6 7 8))
(setq current '( 1 3 5 7 8 6 4 2 0))

(defun up (current zeroPos)
  (let ((aux (copy-list current)))
    ;(if (and (> zeroPos 2) (<= zeroPos 8))
    (rotatef (elt aux zeroPos) (elt aux (- zeroPos 3)))
    aux
  )
)

(defun down (current zeroPos)
  (let ((aux (copy-list current)))
    ;(if (and (<= zeroPos 5))
    (rotatef (elt aux zeroPos) (elt aux (+ zeroPos 3)))
    aux
  )
)

(defun right (current zeroPos)
  (let ((aux (copy-list current)))
    ;(if (and (not (or (= zeroPos 2) (= zeroPos 5) (= zeroPos 8))) (<= zeroPos 8))
    (rotatef (elt aux zeroPos) (elt aux (+ zeroPos 1)))
    aux
  )
)

(defun left (current zeroPos)
  (let ((aux (copy-list current)))
    ;(if (and (not (or (= (position 0 current) 0) (= (position 0 current) 3) (= (position 0 current) 6))) (<= (position 0 current) 8))
    (rotatef (elt aux zeroPos) (elt aux (- zeroPos 1)))
    aux
  )
)

(defun getTotalCost (current)
    (if (not (null current) ) 
        (+ (getTotalCost (cdr current ) ) (getCost  (- 9 (length current) ) (car current) ) )  
        0
    )
)
;versiónn usando var globa. DEPRECATED
(setq varTotalCost 0)
(defun getTotalCost2 (current)
    (cond 
        ((null current))
        (t (incf varTotalCost (getCost  (- 9 (length current)) (car current) ) ) (getTotalCost2 (cdr current)))
    )
  varTotalCost
)

; Asigna a q y a r la división entera y el módulo, respectivamente, de index (la posición actual del número) entre 3
; Asigna a q2 y a r2 la división entera y el módulo, respectivamente, de goalIndex (a donde lo queremos mover) entre 3
; Hace la resta de q - q2 para calcular los movimientos en vertical
; Hace la resta de r - r2 para calcular los movimientos en horizontal
; La suma de estos dos da el costo
(defun getCost (index goalIndex)
    (+ (abs (- (multiple-value-bind (q r) (floor index 3) q) (multiple-value-bind (q2 r2) (floor goalIndex 3) q2)))  (abs (- (multiple-value-bind (q r) (floor index 3) r) (multiple-value-bind (q2 r2) (floor goalIndex 3) r2))))
)

; Current es el estado actual de la lista
; nodeNumber es el contador del estado
; father es el estado del que proviene
; level es la profundidad 
; cost es la suma del costo desde el inicio
; direction es la dirección de la que viene
; CORRER CON : (manhattan current 1 0 0 0 nil)

;(setq nodeNumber 1)
;(setq father 0)
;(setq level 0)
;(setq cost 0)
;(setq direction nil)
;(position 0 current) = posición del cero
(setq cont 0)
(setq rightCost 0)
(setq leftCost 0)
(setq upCost 0)
(setq downCost 0)
(setq rightMan 0)
(defun manhattan (current nodeNumber father level cost direction)

  ;checa si puede mover a la izquierda, en dado caso lo hace
  (if (and (not (or (= (position 0 current) 0) (= (position 0 current) 3) (= (position 0 current) 6))) (<= (position 0 current) 8))
    (setq leftCost (getTotalCost (left current (position 0 current))))
  )
    ;(manhattan (left current (position 0 current)) (+ nodeNumber 1) nodeNumber (+ level 1) (+ cost (getTotalCost current)) 1) 
  
  ;checa si puede mover a la derecha, en dado caso lo hace
  (if (and (not (or (= (position 0 current) 2) (= (position 0 current) 5) (= (position 0 current) 8))) (<= (position 0 current) 8))
    (setq rightCost (getTotalCost (right current (position 0 current))))
  )

  ;checa si puede mover abajo, en dado caso lo hace
  (if (and (<= (position 0 current) 5))
    (setq downCost (getTotalCost (down current (position 0 current))))
  )

  ;checa si puede mover arriba, en dado caso lo hace
  (if (and (> (position 0 current) 2) (<= (position 0 current) 8))
    (setq upCost (getTotalCost (up current (position 0 current))))
  )
)