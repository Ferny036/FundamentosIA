; Nodo: (id estado ancestro operador)
;           L O L B   L O L B
; Estado: ((1 1 1 1) (0 0 0 0))

(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '(
  (:No-Llevar-a-Nadie  (0 0 0))
  (:Llevar-Lobo        (1 0 0))
  (:Llevar-Oveja       (0 1 0))
  (:Llevar-Legumbre    (0 0 1))))

(defparameter  *id* 0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil) 

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

(defun  barge-shore (estado)
    (if  (= 1 (fourth (first  estado)))  0  1))

(defun valid-operator? (op estado)
  (let* (
    (orilla (barge-shore  estado))
    (lobo (first (nth orilla estado)))
    (oveja (second (nth orilla estado)))
    (legumbre (third (nth orilla estado))))

    (and 
      (>= lobo (first (second op)))
      (>= oveja (second (second op)))
      (>= legumbre (third (second op))))))

(defun flip (bit)  
  (boole  BOOLE-XOR  bit  1))

; Filtro de validacion de estado
(defun valid-state? (estado)
  (let* (
    (orilla (flip (barge-shore estado)))
    (p-lobo (= 1 (first (nth orilla estado))))
    (p-oveja (= 1 (second (nth orilla estado))))
    (p-legumbre (= 1 (third (nth orilla estado)))))

    (not (and p-oveja (not (equal p-lobo p-legumbre))))))

(defun apply-operator (op estado)
  (let* ( 
    (lo0 (first (first estado)))
    (lo1 (first (second estado)))
    (o0 (second (first estado)))
    (o1 (second (second estado)))
    (le0 (third (first estado)))
    (le1 (third (second estado)))
    (b0 (fourth (first estado)))
    (b1 (fourth (second estado)))
    (orilla-barca  (barge-shore estado)) 
    (operador (first op)))
    
    (case operador
      (:No-Llevar-a-Nadie 
        (if (= 0 orilla-barca) 
          (list  (list  lo0 o0 le0 (flip b0))   (list  lo1 o1 le1 (flip b1))) 
          (list  (list  lo0 o0 le0 (flip b0))   (list  lo1 o1 le1 (flip b1))))) 
      (:Llevar-Lobo 
        (if (= 0 orilla-barca) 
          (list  (list  (- lo0 1) o0 le0 (flip b0))   (list  (+ lo1 1) o1 le1 (flip b1))) 
          (list  (list  (+ lo0 1) o0 le0 (flip b0))   (list  (- lo1 1) o1 le1 (flip b1))))) 
      (:Llevar-Oveja
        (if (= 0 orilla-barca) 
          (list  (list  lo0 (- o0 1) le0 (flip b0))   (list  lo1 (+ o1 1) le1 (flip b1))) 
          (list  (list  lo0 (+ o0 1) le0 (flip b0))   (list  lo1 (- o1 1) le1 (flip b1)))))
      (:Llevar-Legumbre
        (if (= 0 orilla-barca) 
          (list  (list  lo0 o0 (- le0 1) (flip b0))   (list  lo1 o1 (+ le1 1) (flip b1))) 
          (list  (list  lo0 o0 (+ le0 1) (flip b0))   (list  lo1 o1 (- le1 1) (flip b1)))))
      (T "Error")
    )))

(defun expand (estado)
  (let (  
    (descendientes  nil)
    (nuevo-estado  nil))
    (dolist  (op  *Ops*  descendientes) 
      (when (and  (valid-operator?  op  estado)           ;; se valida el resultado...
                  (valid-state?  (setq nuevo-estado (apply-operator op estado))))
        (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

(defun  remember-state?  (estado  lista-memoria)  
  (cond 
    ((null  lista-memoria)  Nil)
    ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
    (T  (remember-state?  estado  (rest  lista-memoria)))))

(defun  filter-memories (lista-estados-y-ops) 
  (cond ((null  lista-estados-y-ops)  Nil)
        ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
          (filter-memories  (rest  lista-estados-y-ops)))
        (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

(defun extract-solution (nodo)
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
  (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
  (let ((nodo  nil))
    (dotimes  (i (length  lista-nodos))
      (setq  nodo  (nth  i  lista-nodos))
      (if  (= i 0)
        (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
        ;;else
        (format t "\(~2A\)  aplicando ~18A se llega a ~22A~%" i (fourth  nodo) (second  nodo)))))) ;; imprimir el número de paso, operador y estado...

(defun reset-globals () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun  blind-search (edo-inicial  edo-meta  metodo)
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