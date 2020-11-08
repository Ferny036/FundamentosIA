; Nodo: (id estado ancestro operador)
; Estado: (<Roca1> <Roca2> <Roca3> <Roca4> <Roca5> <Roca6> <Roca7>)

(defstruct rana color)
(defparameter *open* '())
(defparameter *memory* '())

(defparameter *ops* '(
  (:Mover-Rana-Una-Posicion-En-Roca-1     (0 1))
  (:Mover-Rana-Una-Posicion-En-Roca-2     (1 1))
  (:Mover-Rana-Una-Posicion-En-Roca-3     (2 1))
  (:Mover-Rana-Una-Posicion-En-Roca-4     (3 1))
  (:Mover-Rana-Una-Posicion-En-Roca-5     (4 1))
  (:Mover-Rana-Una-Posicion-En-Roca-6     (5 1))
  (:Mover-Rana-Una-Posicion-En-Roca-7     (6 1))
  (:Mover-Rana-Dos-Posiciones-En-Roca-1   (0 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-2   (1 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-3   (2 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-4   (3 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-5   (4 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-6   (5 2))
  (:Mover-Rana-Dos-Posiciones-En-Roca-7   (6 2))
  (:Mover-Rana-Tres-Posiciones-En-Roca-1  (0 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-2  (1 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-3  (2 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-4  (3 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-5  (4 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-6  (5 3))
  (:Mover-Rana-Tres-Posiciones-En-Roca-7  (6 3))))

(defparameter *current-ancestor* NIL)
(defparameter *id* 0)
(defparameter *solucion* NIL)

(defun create-node (estado op)
  (incf *id*)
  (list *id* estado *current-ancestor* (first op)))

(defun insert-to-open (estado op metodo)
  (let ((nodo (create-node estado op)))
    (cond ((eql metodo :depth-first) 
            (push nodo *open*))
          ((eql metodo :breath-first) 
            (setq *open* (append *open* (list nodo))))
          (T NIL))))

(defun get-from-open ()
  (pop *open*))

; Filtros
; Filtro de validacion de operadores
(defun valid-operator? (op estado)
  (let* (
    (saltos (second (second op)))
    (roca (first (second op)))
    (rana-actual (nth roca estado)))

    (and 
      (rana-p rana-actual)
      (if (equal :Verde (rana-color rana-actual)) 
        (>= (- roca saltos) 0) 
        (<= (+ roca saltos) 6)))))

; Filtro de validacion de estado
(defun valid-state? (estado)
  (let ((num-ranas (loop for x-rana in estado count (rana-p x-rana))))
    (= num-ranas 6)))

(defun roca-final (roca-actual saltos color)
  (if (equal color :Verde) (- roca-actual saltos) (+ roca-actual saltos)))

(defun apply-operator (op estado)
  (let* (
    (saltos (second (second op)))
    (roca (first (second op)))
    (rana-actual (nth roca estado))
    (color (rana-color rana-actual))
    (roca-a-mover (roca-final roca saltos color))
    (nuevo-estado (loop for x in estado collect x)))

    (setf (nth roca-a-mover nuevo-estado) rana-actual)
    (setf (nth roca nuevo-estado) NIL)
    nuevo-estado))

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
  (let (  (descendientes  nil)
          (nuevo-estado  nil))

    (dolist  (op  *Ops*  descendientes) 
      (when (and  (valid-operator?  op  estado)           ;; se valida el resultado...
                  (valid-state?  (setq nuevo-estado (apply-operator op estado))))
        (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))


(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
  el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
  el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
  (cond 
    ((null  lista-memoria)  Nil)
    ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
    (T  (remember-state?  estado  (rest  lista-memoria)))))

(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
  la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
  (cond ((null  lista-estados-y-ops)  Nil)
        ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
          (filter-memories  (rest  lista-estados-y-ops)))
        (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
  (labels (
    (locate-node  (id lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
      (cond ((null  lista)  Nil)
        ((eql  id  (first (first  lista))) (first  lista))
        (T  (locate-node  id (rest  lista))))))
  
    (let ((current (locate-node (first nodo) *memory*)))
      (loop  while  (not (null  current))  do                        
        (push  current  *solucion*)     ;; agregar a la solución el nodo actual
        (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
          *solucion*))

(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
  (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
  (let ((nodo  nil))
    (dotimes  (i (length  lista-nodos))
      (setq  nodo  (nth  i  lista-nodos))
      (if  (= i 0)
        (format t "Inicio en: ~A~%" (imprimirEstado (second  nodo)))  ;; a partir de este estado inicial
        ;;else
        (format t "\(~2A\)  aplicando ~35A se llega a ~26A~%" i (fourth  nodo) (imprimirEstado (second  nodo)))))))  ;; imprimir el número de paso, operador y estado...

(defun reset-globals () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun imprimirEstado (estado)
  (let ((colores ()))
    (loop for frog in estado 
      if (rana-p frog) do (push (rana-color frog) colores) 
      else do (push nil colores))
      (reverse colores)))

(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son: :depth-first - búsqueda en profundidad
                              :breath-first - búsqueda en anchura"
  (reset-globals)
  (let (
    (nodo nil)
    (estado nil)
    (sucesores  '())
    (operador  nil)
    (meta-encontrada  nil))

    (insert-to-open edo-inicial nil metodo)
    (loop until (or  meta-encontrada (null *open*))  do
      (setq nodo (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
            estado (second  nodo)               ;;Identificar el estado y operador que contiene
            operador (third  nodo))             
      (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
      (cond 
        ((equalp edo-meta  estado)
          (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
          (display-solution  (extract-solution nodo))
          (setq  meta-encontrada  T))
        (t 
          (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
          (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
          (loop for  element  in  sucesores  do
            (insert-to-open  (first element)  (second element)  metodo)))))))
