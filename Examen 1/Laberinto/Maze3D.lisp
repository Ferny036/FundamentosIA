(load "maze_lib.lisp")

(add-algorithm 'Depth-First-Search)
(add-algorithm 'Best-First-Search)
(add-algorithm 'A*)

(defparameter *open* '())   ;;; Frontera de Busqueda
(defparameter *memory* '()) ;;; Memoria de intentos previos
(defparameter *nivel* 0)

;;; Operadores 
(defparameter  *ops*  '(
  (:Arriba            0 (0 -1))
  (:Derecha           2 (1 0))  
  (:Abajo             4 (0 1))
  (:Izquierda         6 (-1 0))))

(defparameter *current-ancestor* NIL) ;;; Id del ancestro comun a todos los descendientes que se generen
(defparameter *id* 0)                 ;;; Identificador del ultimo nodo creado

(defun h (estado)
  (+  (abs (- (aref *goal* 0) (second estado))) 
        (abs (- (aref *goal* 1) (first estado)))))

(defun g (estado)
  (+  (abs (- (aref *start* 0) (second estado))) 
        (abs (- (aref *start* 1) (first estado)))
      *nivel*))

(defun f (estado)
  (+ (h estado) (g estado)))

(defun aptitude (estado metodo)
  (cond ((eql metodo :A*) (f estado))
        ((eql metodo :best-first-search) (h estado))
        (T NIL)))

(defun create-node (estado op metodo)
  (incf *id*)
  (list *id* estado *current-ancestor*  op (aptitude (first estado) metodo)))

(defun insertion (nodo lista)
  (cond ((null lista) (list nodo))
        ((< (fifth nodo) (fifth (first lista))) (cons nodo lista))
        (T (cons (first lista) (insertion nodo (rest lista))))))

(defun remove-states (nodo lista)
  (cond 
    ((null lista) nil) 
    ((equal (second nodo) (second (first lista))) (remove-states nodo (rest lista)))    
    (T (cons (first lista) (remove-states nodo (rest lista))))))

(defun insert (nodo lista)
  (cond ((null lista) nil)
        ((equal (second nodo) (second (first lista))) 
          (if (< (fifth nodo) (fifth (first lista))) 
            (cons nodo (remove-states nodo lista)) 
            (cons (first lista) (remove-states nodo lista))))
        (T (cons (first lista) (insert nodo (rest lista))))))

(defun isState? (estado lista)
  (cond ((null lista) NIL)
        ((equal estado (second (first lista))) T)
        (T (isState? estado (rest lista)))))

(defun insert-to-open (estado op metodo)
  (let ((nodo (create-node estado op metodo)))
    (cond
      ((eql metodo :depth-first)
        (push nodo *open*))
      (T (if (isState? estado *open*)
          (setq *open* (insert nodo *open*))
          (setq *open* (insertion nodo *open*)))))))

(defun get-from-open ()
  (pop *open*))

(defun wasUp-or-wasDown? (up? op walls)
  (case op
    ((0 4) (if (= 16 walls) (not up?) up?))
    ((2 6) (if (= 16 walls) up? (not up?)))))

(defun wall-on-sites? (op walls)
  (= 0 (logand walls (expt 2 (/ op 2)))))

(defun valid-operator? (op estado)
  (let* (
    (x (first (first estado)))
    (y (second (first estado)))
    (columns (1- (get-maze-cols)))
    (rows (1- (get-maze-rows)))
    (pos (get-cell-walls y x))
    (flag T))
    (if (<= 16 pos) 
      (setq flag (wasUp-or-wasDown? (second estado) op pos)) 
      (setq flag (wall-on-sites? op pos)))
    (case op 
      (0 (and flag (<= 0 (1- y))))
      (2 (and flag (<= (1+ x) columns)))
      (4 (and flag (<= (1+ y) rows)))
      (6 (and flag (<= 0 (1- x)))))))

(defun valid-state? (estado op) T)


(defun isUp? (op walls)
  (if (< walls 16) 
    NIL 
    (case op
    ((0 4) (if (= walls 16) NIL T))
    ((2 6) (if (= walls 16) T NIL)))))

(defun apply-operator (op estado)
  (let ((nvo-edo (mapcar #'+ estado (third op))))
        (list nvo-edo (isUp? (second op) (get-cell-walls (second nvo-edo) (first nvo-edo))))))

(defun expand (estado)
  (let (  
    (descendientes  nil)
    (nuevo-estado  nil))
    (dolist  (op  *ops*  descendientes) 
      (when (and  (valid-operator?  (second op) estado)           ;; se valida el resultado...
                  (valid-state?  (setq nuevo-estado (apply-operator op (first estado))) (second op)))
        (setq  descendientes  (cons  (list nuevo-estado (second op)) descendientes))))))

(defun  remember-state?  (estado  lista-memoria)
  (cond 
    ((null  lista-memoria)  Nil)
    ((equalp  estado  (second (first  lista-memoria)))  T)
    (T  (remember-state?  estado  (rest  lista-memoria)))))

(defun filter-memories (lista-estados-y-ops) 
  (cond ((null  lista-estados-y-ops)  Nil)
        ((remember-state? (first (first  lista-estados-y-ops)) *memory*) (filter-memories  (rest  lista-estados-y-ops)))
        (T  (cons (first lista-estados-y-ops) (filter-memories (rest lista-estados-y-ops))))))

(defun extract-solution (nodo)
  (labels (
    (locate-node  (id lista)
      (cond ((null  lista)  Nil)
        ((eql  id  (first (first  lista))) (first  lista))
        (T  (locate-node  id (rest  lista))))))
  
    (let ((current (locate-node (first nodo) *memory*)))
      (loop  while  (not (null  current))  do                        
        (push (fourth current)  *solution*)
        (setq current  (locate-node  (third  current) *memory*)))
      (pop *solution*))))

(defun reset-globals ()
  (setq *open* '())
  (setq *memory* '())
  (setq *solution* '())
  (setq *current-ancestor* NIL) ;;; Id del ancestro comun a todos los descendientes que se generen
  (setq *id* 0)
  (setq *nivel* 0))

(defun isGoal? (estado)
  (equalp (first estado) (list (aref *goal* 1) (aref *goal* 0))))

(defun Informed-Search (metodo)
  (reset-globals)
  (let (
    (nodo nil)
    (estado nil)
    (sucesores '())
    (meta-encontrada nil))

    (insert-to-open (list (list (aref *start* 1) (aref *start* 0)) NIL) NIL metodo)
    (loop until (or meta-encontrada (null *open*)) do
      (setq nodo (get-from-open)
            estado (second nodo))
      (push nodo *memory*)
      (cond ( (isGoal? estado)
              (setq *open* '())
              (extract-solution nodo)
              (setq meta-encontrada T))
            (T 
              (incf *nivel*)
              (setq *current-ancestor* (first nodo))
              (setq sucesores (filter-memories (expand estado)))
              (loop for element in sucesores do
                (insert-to-open (first element) (second element) metodo)))))))


(defun A* ()
  (Informed-Search :A*))

(defun Best-First-Search ()
  (Informed-Search :best-first-search))

(defun Depth-First-Search ()
  (Informed-Search :depth-first))

(start-maze)