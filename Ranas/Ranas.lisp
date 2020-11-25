;;;=========================================================================================================================;;
;;;  Ranas.lisp                                                                                                             ;;
;;;      Resuelve el problema de las 6 ranas en un estanque con búsqueda ciega, a lo profundo y a lo ancho.                 ;;    
;;;                                                                                                                         ;;
;;;      Representación de los estados:                                                                                     ;;
;;;         Lista de registros del tipo Rana.                                                                               ;;
;;;         Cada posicion representa una roca del estanque.                                                                 ;;
;;;         En cada Roca puede haber o no una Rana (NIL sera el valor para indicar que en esa Roca no hay una Rana)         ;;
;;;         Hay dos tipos de Ranas (Verdes y Cafes)                                                                         ;;
;;;                 Estado incial:  (Cafe Cafe Cafe NIL Verde Verde Verde)                                                  ;;
;;;                 Estado meta:    (Verde Verde Verde NIL Cafe Cafe Cafe)                                                  ;;
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
;;; ( < nombre > ( < roca origen > < saltos > ) )
;;;   < nombre >: Operador descrito en lenguaje natural
;;;   < roca origen >: Posicion de la roca actual  
;;;   < saltos >: Cantidad de saltos a realizar desde roca actual

;;; Estructura del estado 
;;; ( < Roca1 > < Roca2 > < Roca3 > < Roca4 > < Roca5 > < Roca6 > < Roca7 > )
;;;   < Roca# >: Valor de la Roca # (Rana ó NIL)

(defparameter *nodos-creados* 0)      ;;; Indicador que cuenta la cantidad de nodos creados
(defparameter *nodos-expandidos* 0)   ;;; Indicador que cuenta la cantidad de nodos que se expandieron
(defparameter *long-max-frontera* 0)  ;;; Indicador que calcula el tamaño máximo de la frontera de búsqueda
(defparameter *long-solucion* 0)      ;;; Indicador que calcula el tamaño de la solución del problema
(defparameter *tiempo-inicial* NIL)   ;;; Indicador que considera el conteo real de ciclos de reloj antes de ejecutar el programa
(defparameter *tiempo-final* NIL)     ;;; Indicador que considera el conteo real de ciclos de reloj despues de ejecutar el programa

(defstruct rana color)      ;;; Estructura Rana a utilizar donde la llave es el color

;;; Estado inicial del problema
(defparameter *estado-inicial*  
  (list
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    NIL
    (make-rana :color :verde)
    (make-rana :color :verde)
    (make-rana :color :verde)))

;;; Estado meta del problema
(defparameter *estado-final*  
  (list
    (make-rana :color :verde)
    (make-rana :color :verde)
    (make-rana :color :verde)
    NIL
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    (make-rana :color :cafe)))

(defparameter *open* '())   ;;; Frontera de Busqueda
(defparameter *memory* '()) ;;; Memoria de intentos previos

;;; Operadores 
(defparameter *ops* '(
  (:Mover-Rana-De-Roca-1-Una-Posicion     (0 1))
  (:Mover-Rana-De-Roca-2-Una-Posicion     (1 1))
  (:Mover-Rana-De-Roca-3-Una-Posicion     (2 1))
  (:Mover-Rana-De-Roca-4-Una-Posicion     (3 1))
  (:Mover-Rana-De-Roca-5-Una-Posicion     (4 1))
  (:Mover-Rana-De-Roca-6-Una-Posicion     (5 1))
  (:Mover-Rana-De-Roca-7-Una-Posicion     (6 1))
  (:Mover-Rana-De-Roca-1-Dos-Posiciones   (0 2))
  (:Mover-Rana-De-Roca-2-Dos-Posiciones   (1 2))
  (:Mover-Rana-De-Roca-3-Dos-Posiciones   (2 2))
  (:Mover-Rana-De-Roca-4-Dos-Posiciones   (3 2))
  (:Mover-Rana-De-Roca-5-Dos-Posiciones   (4 2))
  (:Mover-Rana-De-Roca-6-Dos-Posiciones   (5 2))
  (:Mover-Rana-De-Roca-7-Dos-Posiciones   (6 2))
  (:Mover-Rana-De-Roca-1-Tres-Posiciones  (0 3))
  (:Mover-Rana-De-Roca-2-Tres-Posiciones  (1 3))
  (:Mover-Rana-De-Roca-3-Tres-Posiciones  (2 3))
  (:Mover-Rana-De-Roca-4-Tres-Posiciones  (3 3))
  (:Mover-Rana-De-Roca-5-Tres-Posiciones  (4 3))
  (:Mover-Rana-De-Roca-6-Tres-Posiciones  (5 3))
  (:Mover-Rana-De-Roca-7-Tres-Posiciones  (6 3))
  ))

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
  (incf *nodos-creados*)
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

#| Predicado. Valid Operator
  
  Valida si el operador se puede aplicar al estado.

  @param op     - Operador a validar.
  @param estado - Estado donde se va a verificar [@param op]
  @return       - T si en la roca actual hay una rana y no salta más alla del limite
|#
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

#| Predicado. Valid State
  
  Valida si estado actual cumple con las restricciones.

  @return       - T si en total hay 6 ranas
|#
(defun valid-state? (estado)
  (let ((num-ranas (loop for x-rana in estado count (rana-p x-rana))))
    (= num-ranas 6)))

#| Funcion. Roca final
  
  Calcula la posicion de la roca donde se moverá la rana actual, de acuerdo a su color.

  @param roca-actual  - Posicion de la roca actual
  @param saltos       - Cantidad de saltos a moverse
  @param color        - Color de la rana actual
  @return             - Posicion de la roca donde se moverá la rana
|#
(defun roca-final (roca-actual saltos color)
  (if (equal color :Verde) (- roca-actual saltos) (+ roca-actual saltos)))

#| Funcion. Apply Operator
  
  Cambia el estado del sistema.

  @param op     - Operador a aplicar sobre [@param estado].
  @param estado - Estado a cambiar.
  @return       - Nuevo estado depués de aplicar [@param op].
|#
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
      (when (and  (valid-operator?  op  estado)
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
  (setf *long-solucion* (length *solucion*))      
  *solucion*))

#| Funcion. Transform state
  
  Transforma un estado en su forma de registro en una lista de elementos de estado (colores de las ranas en la roca).

  @param estado - Estado a transformar.
  @return       - Lista de elementos cuyos elementos son los colores de las ranas pertenecientes a [@param estado]. 
|#
(defun transform-estate (estado)
  (let ((colores ()))
    (loop for frog in estado 
      if (rana-p frog) do (push (rana-color frog) colores) 
      else do (push nil colores))
      (reverse colores)))

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
        (format t "Inicio en: ~A~%" (transform-estate (second  nodo)))
        ;;else
        (format t "\(~2A\)  aplicando ~40A se llega a ~26A~%" i (fourth  nodo) (transform-estate (second  nodo)))))))

#| Funcion. Display solution
  
  Reinicia todas las variables globales para realizar una nueva búsqueda.

|#
(defun reset-globals () 
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solucion*  nil)
    (setf *nodos-creados* 0)
    (setf *nodos-expandidos* 0)
    (setf *long-solucion* 0)
    (setf *long-max-frontera* 0))

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
          (incf  *nodos-expandidos*)
          (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
          (setf  *long-max-frontera* (if (> (length *open*) *long-max-frontera*) (length *open*) *long-max-frontera*))
          (setq  sucesores  (filter-memories  sucesores))
          (loop for  element  in  sucesores  do
            (insert-to-open  (first element)  (second element)  metodo)))))))

#| Funcion. Calcular Tiempo
  
  Utiliza los parametros globales para determinar el tiempo total de ejecucion del programa:  

  @return Calculo del tiempo en segundos de ejecucion del programa.
|#
(defun calcularTiempo ()
  (/ (- *tiempo-final* *tiempo-inicial*) internal-time-units-per-second))

#| Funcion. Imprimir Indicadores
  
  Imprime los cinco indicadores requeridos:
    * Nodos Creados
    * Nodos Expandidos
    * Longitud maxima de la frontera de busqueda
    * Longitud de la solucion
    * Tiempo para encontrar la solucion
|#
(defun imprimirIndicadores ()
  (format  t  "~%~%Nodos Creados: ~A~%Nodos Expandidos: ~A~%Longitud máxima de la Frontera de búsqueda: ~A~%Longitud de la solución: ~A~%Tiempo para encontrar la solución: ~A seg.~%" 
    *nodos-creados*
    *nodos-expandidos*
    *long-max-frontera*
    *long-solucion*
    (float (calcularTiempo))))

#| Funcion. Main
  
  Funcion principal del archivo, llama a la funcion de busqueda ciega mientras genera los indicadores globales
  
  @param metodo - Indica el metodo a utilizar, desde un estado inicial hasta un estado meta, 
  los métodos posibles son:  
    * :depth-first - Búsqueda en profundidad
    * :breath-first - Búsqueda en anchura
|#
(defun main (metodo)
  (setf *tiempo-inicial* (get-internal-real-time))
  (blind-search *estado-inicial*  *estado-final*  metodo)
  (setf *tiempo-final* (get-internal-real-time))
  (imprimirIndicadores))