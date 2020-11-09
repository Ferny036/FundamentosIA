;;;=========================================================================================================================;;
;;;  GLOL.lisp                                                                                                              ;;
;;;      Resuelve el problema del granjero, el lobo, la oveja y la legumbre con búsqueda ciega, a lo profundo y a lo ancho. ;;    
;;;                                                                                                                         ;;
;;;      Representación de los estados:                                                                                     ;;
;;;         Lista con dos sublistas internas, una por cada orilla.                                                          ;;
;;;         En cada orilla, Lobo (Lo), Oveja (O), Legumbre (Le) y Barca(B).                                                 ;;
;;;                                                                                                                         ;;
;;;                 Estado incial:                  Estado meta:                                                            ;;
;;;            Lo O  Le B   Lo O  Le  B       Lo O  Le B   Lo O  Le  B                                                      ;;
;;;          ((1  1  1  1) (0  0  0  0))    ((0  0  0  0) (1  1  1  1))                                                     ;;  
;;;                                                                                                                         ;;
;;;      Rivera Paredes Fernando Daniel                                                                                     ;;
;;;  Noviembre, 2020                                                                                                        ;;
;;;=========================================================================================================================;;

;;; Estructura del nodo
;;; (< id > < estado > < current-ancestor > < operator >)
;;;   < id >: Identificador del nodo
;;;   < estado >: Estado actual del nodo
;;;   < current-ancestor >: Id del ancestro del nodo
;;;   < operator >: Operador aplicado para obtener el estado

;;; Estructura "incompleta" del nodo
;;; (< estado > < operator >)
;;;   < estado >: Estado actual del nodo
;;;   < operator >: Operador aplicado para obtener <estado>

;;; Estructura de los operadores
;;; ( < nombre > ( < lobo > < oveja > < legumbre > ) )
;;;   < nombre >: Operador descrito en lenguaje natural
;;;   < lobo >: Cantidad de "lobo" a mover 
;;;   < oveja >: Cantidad de "oveja" a mover
;;;   < legumbre >: Cantidad de "legumbre" a mover

;;; Estructura del estado 
;;; (( < lobo0 > < oveja0 > < legumbre0 > < barca0 > ) (< lobo1 > < oveja1 > < legumbre1 > < barca1 >)) 
;;;   < lobo# >: Cantidad de "lobo" en la orilla #
;;;   < oveja# >: Cantidad de "oveja" en la orilla #
;;;   < legumbre# >: Cantidad de "legumbre" en la orilla #
;;;   < barca# >: Cantidad de "barca" en la orilla #

(defparameter *open* '())   ;;; Frontera de Busqueda
(defparameter *memory* '()) ;;; Memoria de intentos previos

;;; Operadores 
(defparameter  *ops*  '(
  (:No-Llevar-a-Nadie  (0 0 0))
  (:Llevar-Lobo        (1 0 0))
  (:Llevar-Oveja       (0 1 0))
  (:Llevar-Legumbre    (0 0 1))))

(defparameter *current-ancestor* NIL) ;;; Id del ancestro comun a todos los descendientes que se generen
(defparameter *id* 0)                 ;;; Identificador del ultimo nodo creado
(defparameter *solucion* NIL)         ;;; Lista donde se almacenara la solucion recuperada de la memoria

#| Funcion. Create Node
  
  Crea un nodo e incrementa el identificador de nodos [*id*]

  @param estado - Un estado el problema a resolver
  @param op     - El operador cuya aplicación generó a [@param estado].
  @return       - Lista cuyos elementos son:
                    * id: Identificador del nodo
                    * estado: Estado actual del nodo
                    * current-ancestor: Id del ancestro del nodo
                    * (first op): Nombre del operador
|#
(defun create-node (estado op)
  (incf *id*)
  (list *id* estado *current-ancestor* (first op)))

#| Funcion. Insert to open
  
  Añade un nodo a la frontera de busqueda [*open*]

  @param estado - Un estado el problema a resolver
  @param op     - El operador cuya aplicación generó a [@param estado].
  @param metodo - Metodo a usar para insertar en la frontera de busqueda
                  :depth-first  - Inserta los elementos de la lista en orden inverso y por el inicio de la lista
                  :breath-first - Inserta los elementos de la lista en orden normal y por el final de la lista
|#
(defun insert-to-open (estado op metodo)
  (let ((nodo (create-node estado op)))
    (cond ((eql metodo :depth-first) 
            (push nodo *open*))
          ((eql metodo :breath-first) 
            (setq *open* (append *open* (list nodo))))
          (T NIL))))

#| Funcion. Get from open
  
  Devuelve el primer elemento de la frontera de busqueda[*open*] de manera destructiva.

  @return       - Primer elemento de la frontera de busqueda (Nodo)
|#
(defun get-from-open ()
  (pop *open*))

#| Funcion. Get from open
  
  Devuelve la orilla del rio en la que se encuentra la barca en el estado actual.
  
  @param estado - Estado actual del sistema.
  @return       - Valor (0 ó 1) de donde se encuentra la barca.
|#
(defun  barge-shore (estado)
    (if  (= 1 (fourth (first  estado)))  0  1))

#| Predicado. Valid Operator
  
  Valida si el operador se puede aplicar al estado.

  @param op     - Operador a validar.
  @param estado - Estado donde se va a verificar [@param op]
  @return       - T si existen los recursos para aplicar [@param op] (hay lobo, oveja y legumbre suficientes).
|#
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

#| Funcion. Flip
  
  Aplica operacion XOR al parametro recibido.

  @param bit  - Valor a aplicar operacion XOR sobre el valor "1"
  @return     - El resultado de la operacion XOR en [@param bit] 
|#
(defun flip (bit)  
  (boole  BOOLE-XOR  bit  1))

#| Predicado. Valid State
  
  Valida si estado actual cumple con las restricciones.

  @return       - T si en total hay 6 ranas
|#
(defun valid-state? (estado)
  (let* (
    (orilla (flip (barge-shore estado)))
    (p-lobo (= 1 (first (nth orilla estado))))
    (p-oveja (= 1 (second (nth orilla estado))))
    (p-legumbre (= 1 (third (nth orilla estado)))))

    (not (and p-oveja (not (equal p-lobo p-legumbre))))))

#| Funcion. Apply Operator
  
  Cambia el estado del sistema.

  @param op     - Operador a aplicar sobre [@param estado].
  @param estado - Estado a cambiar.
  @return       - Nuevo estado depués de aplicar [@param op].
|#
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

#| Funcion. Expand
  
  Devuelve una lista de descendientes válidos del estado actual, operando sobre todos los elementos de [*ops*].

  @param estado - Estado actual del sistema.
  @return       - Lista de descendientes que pasaron los dos primeros filtros (Filtro de operador y estado).
|#
(defun expand (estado)
  (let (  
    (descendientes  nil)
    (nuevo-estado  nil))
    (dolist  (op  *Ops*  descendientes) 
      (when (and  (valid-operator?  op  estado)           ;; se valida el resultado...
                  (valid-state?  (setq nuevo-estado (apply-operator op estado))))
        (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

#| Predicado (Recursivo). Remember state
  
  Valida si el estado actual ya se encuentra en [*memory*].

  @param estado         - Estado actual del sistema.
  @param lista-memoria  - Memoria del sistema [*memory*].
  @return               - T si el estado se encuentra en *memory*.
|#
(defun  remember-state?  (estado  lista-memoria)
  (cond 
    ((null  lista-memoria)  Nil)
    ((equal  estado  (second (first  lista-memoria)))  T)
    (T  (remember-state?  estado  (rest  lista-memoria)))))

#| Funcion (Recursiva). Filter memories
  
  Filtra los nodos, tales que, el estado almacenado en el nodo se encuentre en la memoria del sistema [*memory*].
  (Los nodos se encuentran representados de forma incompleta, contando solo con el estado y su operador)

  @param lista-estados-y-ops  - Lista donde se encuentran los nodos (incompletos) a filtrar.
  @return                     - Lista de nodos(incompletos) filtrados 
|#
(defun filter-memories (lista-estados-y-ops) 
  (cond ((null  lista-estados-y-ops)  Nil)
        ((remember-state? (first (first  lista-estados-y-ops)) *memory*)
          (filter-memories  (rest  lista-estados-y-ops)))
        (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))))

#| Funcion. Extract solution
  
  Rastrea en *memory* todos los descendientes del nodo actual hasta llegar al estado inicial.

  @param nodo  - Nodo de donde iniciara la busqueda de descendientes.
  @return      - Lista de nodos descendientes de [@param nodo]. 
|#
(defun extract-solution (nodo)
  (labels (
    (locate-node  (id lista)
      (cond ((null  lista)  Nil)
        ((eql  id  (first (first  lista))) (first  lista))
        (T  (locate-node  id (rest  lista))))))
  
    (let ((current (locate-node (first nodo) *memory*)))
      (loop  while  (not (null  current))  do                        
        (push  current  *solucion*)
        (setq  current  (locate-node  (third  current) *memory*))))
          *solucion*))

#| Funcion. Display solution
  
  Imprime todos los pasos a seguir de la solucion.

  @param lista-nodos - Lista de nodos pertenecientes a la solucion del problema.
|#
(defun  display-solution (lista-nodos)
  (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
  (let ((nodo  nil))
    (dotimes  (i (length  lista-nodos))
      (setq  nodo  (nth  i  lista-nodos))
      (if  (= i 0)
        (format t "Inicio en: ~A~%" (second  nodo))
        ;;else
        (format t "\(~2A\)  aplicando ~18A se llega a ~22A~%" i (fourth  nodo) (second  nodo))))))

#| Funcion. Display solution
  
  Reinicia todas las variables globales para realizar una nueva búsqueda.

|#
(defun reset-globals () 
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))


#| Funcion. Blind search
  
  Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta, 
  los métodos posibles son:  
    * :depth-first - Búsqueda en profundidad
    * :breath-first - Búsqueda en anchura

  @param edo-inicial - Estado inicial del sistema.
  @param edo-final   - Estado meta del sistema.
  @param metodo      - Método a utilizar (:breath-first ó :depth-first).
|#
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
      (setq nodo (get-from-open)           
            estado (second  nodo)              
            operador (third  nodo))             
      (push  nodo  *memory*)                    
      (cond 
        ((equalp edo-meta  estado)
          (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
          (display-solution  (extract-solution nodo))
          (setq  meta-encontrada  T))
        (t 
          (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
          (setq  sucesores  (filter-memories  sucesores))
          (loop for  element  in  sucesores  do
            (insert-to-open  (first element)  (second element)  metodo)))))))