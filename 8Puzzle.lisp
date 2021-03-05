;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions to simplify testing ;;;;;;;;;;;;;;;;;;;;;;

(setq goal '(0 1 2 3 4 5 6 7 8))
(setq test1 '(1 3 5 7 8 6 4 2 0)) ; estado solvible: costo 1715
(setq test2 '(0 1 2 3 4 8 5 7 6)) ; estado solvible: 286
(setq test3 '(1 0 2 3 4 6 5 7 8)) ; estado insolvible
(setq test4 '(1 2 3 4 0 5 6 7 8)) ; estado solvible: costo 282
(setq test5 '(3 1 2 4 7 5 6 0 8)) ; estado solvible: costo 10
(setq test6 '(3 1 2 4 7 5 0 6 8)) ; estado solvible: costo 12

;CORRER CON: (solver test5 'manhattan-total-cost)

(defun solver (state heuristic)
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

;;;;;;;;;;;;; Graph Search ;;;;;;;;;;;;;;;

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
    ))

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
    (action-sequence (node-parent node) (cons (node-action node) actions))
    actions
    ))

(defstruct node 
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (heuristic 0)
  (depth 0))

(defvar *nodes-expanded*)

; Key for Priority Queue
(defun priority-queue-key(node)
  (+ (node-path-cost node) (node-heuristic node))
)

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
    ))

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
            ))
    ))

;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front (LIFO, or stack), to the back (FIFO, or simple queue), or by priority (priority queue).
;;; This is done with the following enqueing functions specified when we make the queue, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-LIFO - elements are a list
;;;   ENQUEUE-FIFO   - elements are a list
;;;   ENQUEUE-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.
;;; For priority queues, we can specify a key function that should return the priority value of an element.
;;; For FIFO queues, we maintain a pointer to the last element for efficient enqueuing.

(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))

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
  q)

;;;; The Three Enqueing Functions

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q)))  ; (nconc x y) is destructive version of (append x y)
  items
  )

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue."
  (if (q-emptyp q) 
      (setf (q-elements q) items)
    (setf (cdr (q-last q)) items))
  (setf (q-last q) (last items))
  items
  )

(defun enqueue-priority (q items)
  "Inserts the items by priority determined by the queue's key function."
  ;; If first insert, create the heap
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (mapc (lambda (item)
	  (heap-insert (q-elements q) item (q-key q)))
	items)
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
