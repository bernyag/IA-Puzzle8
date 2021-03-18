

(setq goal '(0 1 2 3 4 5 6 7 8))  ; estado meta
(setq test1 '(1 3 5 7 8 6 4 2 0)) ; estado solvible: costo 1715
(setq test2 '(0 1 2 3 4 8 5 7 6)) ; estado solvible: 286
(setq test3 '(1 0 2 3 4 6 5 7 8)) ; estado insolvible
(setq test4 '(1 2 3 4 0 5 6 7 8)) ; estado solvible: costo 282
(setq test5 '(3 1 2 4 7 5 6 0 8)) ; estado solvible: costo 10
(setq test6 '(3 1 2 4 7 5 0 6 8)) ; estado solvible: costo 12


;funcion que calcula el costo manhattan recursiva 
; Distancia de manhattan heurística
; Representamos nuestro puzzle de forma que el número 7 tenga el índice 7:
; | indice/3 - númerodepieza/3 | =número de movimientos verticales para que esté en la fila correcta
; | índice%3 - númerodepieza%3 | = número de movimientos horizontales para estar en la columna correcta
; La suma de las dos anteriores, es la distancia de manhattan.
; La funcion "floor" regresa la división y el módulo 
; Nuestro origen será el 0,0, es decir la posición de la esquina superior izquierda
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

;REPRESENTACIÓN DEL PROBLEMA:

; Dado (a b c d e 0 f g h), donde 0 es el espacio en blanco y a b c d e f g h son las otras piezas (o los números), 
; esto corresponde al número 5, que es la ubicación de la pieza 0. La ubicación de las demás piezas no son importantes por ahora.

; Para la representación de los estados, es una lista linear con las ubicaciones de cada pieza en el tablero.
; Por ejemplo, (0 1 2 3 4 5 6 7 8) es la lista que le corresponde a el siguiente tablero:
; 0  1  2
; 3  4  5
; 6  7  8
; Los siguientes estados están representados por la función de "get-next-positions"
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
	(u (list 'U (up current blank)))
	(d (list 'D (down current blank)))
	(l (list 'L (left current blank)))
	(r (list 'R (right current blank))))
    (filter (list u d l r) current)
  )
)


(defun filter (moves current)
  (cond
    ((null moves) nil)
    ((equal (cadar moves) current) (filter (cdr moves) current))
    (t (cons (car moves) (filter (cdr moves) current)))
  )
)

;funciones que permiten hacer movimientos con sus respectivas validaciones
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
(defvar open nil)
(defvar closed nil)
(defvar goal (list 0 1 2 3 4 5 6 7 8))
(defvar counter 1)

(defun init (lstgui)
  ;(setf nodo0 (list 1 0 0 0 NIL (list 8 6 7 2 5 4 3 0 1))) ;dificil
  (setf nodo0 (list 1 0 0 0 NIL lstgui)) ;facil
  (setf open nil)
  (setf closed nil)
  (setf backtrack nil)
  (setf backtracker nil)
  (setf counter 1)
  (setf (fourth nodo0) (manhattan-total-cost (get-current nodo0)))
  (setq flag 0)
  (push nodo0 closed)
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
      (incf counter)
      (setf (first child) counter)
      (setf (second child) (first node))
      (setf (third child) (incf (third child)))
      (setf (fifth child) (caar positions))
      (setf (sixth child) (cadar positions))
      (setf (fourth child) (+ (manhattan-total-cost (get-current child)) (fourth node) ))
      (setf positions (cdr positions))
       (cond
          ((some #'is-true (mapcar #'(lambda (x) (equal (sixth child) (sixth x))) closed )))
          (t 
            (cond
              ((some #'is-true (mapcar #'(lambda (x) (equal (sixth child) (sixth x))) open )))
              (t (push child open)
              ))))    
      (incf flag)
      (expand node)
)) (setq flag 0))

;resuelve el problema, mandando a open y closed
(defun solver ()
  (init lstgui)
  (expand nodo0)
  (dotimes (n 1000000)
    (sort-list)
    (if (> (length open) 200) (setf open (subseq open 0 100)) 0)
    (setf next (car open))
    (push (pop open) closed)
    (if (is-goal (sixth next)) (return t) 0)
    (expand next)
  )
)
; funcion de backtracking 
(defun backtrack (father-id list)
  (setf closed-list (copy-list list))
  (if (not (null closed-list) )
    (if (= (caar closed-list) father-id)
      (progn
        ;(push (car closed-list) backtrack ) ;habilitar si quieres toda la info
        (push (fifth (car closed-list)) backtracker)
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
    (sort open 'compare-cost) 'compare-depth)
)

(defun todo()
    (guardaLista)
    (setq lstgui listaInicial)
    (solver)
    (backtrack (caar closed) closed)
    (cdr backtracker)
    
)

;convertir string de lectura a una lista (de internet)
(defun string-to-list (str)
  (if (not (streamp str))
    (string-to-list (make-string-input-stream str))
      (if (listen str)
      (cons (read str) (string-to-list str))
      nil))
)

;leer archivo donde viene lista de java
(defun leerArchivo()
  (setq infile (open "estado.txt"))
  (setq datastring (read-line infile))
) 

;guardar lista en lisp
(defun guardaLista()
  (setq listaInicial '())
  (leerArchivo)
  (setq listaInicial (car (string-to-list dataString)))
)

(print (setq res (todo)))