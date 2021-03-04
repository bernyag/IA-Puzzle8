(setq goal '(0 1 2 3 4 5 6 7 8))
(setq test1 '(1 3 5 7 8 6 4 2 0)) ; estado solvible: costo 1715
(setq test2 '(0 1 2 3 4 6 5 7 8)) ; estado insolvible
(setq test3 '(1 0 2 3 4 6 5 7 8)) ; estado insolvible
(setq test4 '(1 2 3 4 0 5 6 7 8)) ; estado solvible: costo 282
(setq test5 '(3 1 2 4 7 5 6 0 8)) ; estado solvible: costo 10
(setq test6 '(3 1 2 4 7 5 0 6 8)) ; estado solvible: costo 12

(defun zeroPos (current)
  (position 0 current)
)

(defun up (current pos)
  (let ((aux (copy-list current)))
    ;(if (and (> zeroPos 2) (<= zeroPos 8))
    (rotatef (elt aux pos) (elt aux (- pos 3)))
    aux
  )
)

(defun down (current pos)
  (let ((aux (copy-list current)))
    ;(if (and (<= zeroPos 5))
    (rotatef (elt aux pos) (elt aux (+ pos 3)))
    aux
  )
)

(defun right (current pos)
  (let ((aux (copy-list current)))
    ;(if (and (not (or (= zeroPos 2) (= zeroPos 5) (= zeroPos 8))) (<= zeroPos 8))
    (rotatef (elt aux pos) (elt aux (+ pos 1)))
    aux
  )
)

(defun left (current pos)
  (let ((aux (copy-list current)))
    ;(if (and (not (or (= (position 0 current) 0) (= (position 0 current) 3) (= (position 0 current) 6))) (<= (position 0 current) 8))
    (rotatef (elt aux pos) (elt aux (- pos 1)))
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

; TODO - Qué hace esto?
(defun filter (moves current)
  (cond
    ((null moves) nil)
    ((equal (cadar moves) current) (filter (cdr moves) current))
    (t (cons (car moves) (filter (cdr moves) current)))
  )
)

;regresa los posibles estados a los que se puede mover dado un estado
(defun next-positions (current)
  (let* ((blank (zeroPos current))
	(u (list 'up (up current blank)))
	(d (list 'down (down current blank)))
	(l (list 'left (left current blank)))
	(r (list 'right (right current blank))))
    (filter (list u d l r) current)
  )
)

; Current es el estado actual de la lista
; nodeNumber es el contador del estado
; father es el estado del que proviene
; level es la profundidad 
; cost es la suma del costo desde el inicio
; direction es la dirección de la que viene
; CORRER CON : (manhattan current 1 0 0 0 nil)
; blank es la posicion del cero

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

  (let ((blank (zeroPos current)))

    ;checa si puede mover a la izquierda, en dado caso lo hace
    (if (and (not (or (= blank 0) (= blank 3) (= blank 6))) (<= blank 8))
      (setq leftCost (getTotalCost (left current blank )))
    )
      ;(manhattan (left current blank ) (+ nodeNumber 1) nodeNumber (+ level 1) (+ cost (getTotalCost current)) 1) 
    
    ;checa si puede mover a la derecha, en dado caso lo hace
    (if (and (not (or (= blank 2) (= blank 5) (= blank 8))) (<= blank 8))
      (setq rightCost (getTotalCost (right current blank )))
    )

    ;checa si puede mover abajo, en dado caso lo hace
    (if (and (<= blank 5))
      (setq downCost (getTotalCost (down current blank )) )
    )

    ;checa si puede mover arriba, en dado caso lo hace
    (if (and (> blank 2) (<= blank 8))
      (setq upCost (getTotalCost (up current blank )))
    )
  )
)

; =============================================================================================
; =============================================================================================
; ================================== Parte copiada, ADAPTAR ===================================
; =============================================================================================
; =============================================================================================

(defvar *nodes-expanded*)

(defstruct node 
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (heuristic 0)
  (depth 0))

(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))


(defun general-search (initial-state successor heuristic goalp
		       &key (samep #'eql)
		            (enqueue #'enqueue-LIFO) 
		            (key #'identity))
  (setf *nodes-expanded* 0)
  (let ((fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (values 
     (graph-search fringe nil successor heuristic goalp samep)
     *nodes-expanded*)
  )
)

(defun graph-search (fringe closed successor heuristic goalp samep)
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node)) 
	     (action-sequence node))
            ((member (node-state node) closed 
		     :test samep :key #'node-state)
	     (graph-search fringe closed successor heuristic goalp samep))
            (t 
	     (let ((successors (expand successor heuristic node)))
	       (setf *nodes-expanded* 
		     (+ *nodes-expanded* (length successors)))
	       (graph-search (q-insert fringe successors)
			     (cons node closed)
			     successor heuristic goalp samep)))
      )
    )
  )
)

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
    (action-sequence (node-parent node) (cons (node-action node) actions))
    actions
    ))

; Successor function returns action-state as cost is 1
(defun expand (successor heuristic node)
  (let ((doubles (funcall successor (node-state node))))
    (mapcar (lambda (action-state)
	      (let ((action (car action-state))
		    (state (cadr action-state)))
		(make-node :state state 
			   :parent node
			   :action action 
			   :path-cost (+ (node-path-cost node) 1)
			   :heuristic (funcall heuristic state)
			   :depth (1+ (node-depth node)))
		))
	    doubles)
    )
)
  
  ;;;; Operations on Queues

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))       ; (length x) works for both lists and arrays with fill-pointers

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))              ; (elt x n) works for both lists and arrays

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
      (pop (q-elements q))             ; (pop x) alters x by removing the car, then returns the item removed
    (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to the queue's enqueuing function."
  (funcall (q-enqueue q) q items)
  q
)

;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, moves heap[i] down where it belongs."
  (unless (heap-leafp heap i)
    (let ((l (heap-left i))
	  (r (heap-right i)))
      (let ((smaller-child (if (and (< r (length heap))
				    (< (heap-val heap r key) (heap-val heap l key)))
			       r l)))
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))    ; (rotatef x y) swaps values of x and y
	  (heapify heap smaller-child key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))        ; (decf x) decrements x
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)       ; (vector-push-extend value array) adds the value to the next
                                      ; available position in the array, incrementing the fill-pointer
                                      ; and increasing the size of the array if necessary.
  (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key)) 
	item)

  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items down in the process."
  (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
	 i)
	(t
	 (setf (elt heap i) (elt heap (heap-parent i)))
	 (heap-find-pos heap (heap-parent i) val key))
	))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))
