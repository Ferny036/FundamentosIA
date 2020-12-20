;;;============================================================================================================
;;;  GATO4X4
;;;      Realiza una busqueda heuristica como agente jugador en el juego de Gato de dimensiones 4x4.
;;;
;;;      Función tictactoe: 
;;;         Debe existir una función llamada tictactoe la cuál recibirá un tablero 4x4 que representa 
;;;			el juego. El argumento de la función entonces, es una lista que contiene los elementos 
;;;			del tablero actual.
;;;                 Ejemplo de argumento a recibir:
;;;						(( NIL NIL NIL NIL)
;;;						 ( NIL NIL  X  NIL)
;;;						 ( NIL NIL NIL NIL)
;;;						 ( NIL NIL NIL NIL))
;;;						-los elementos de la lista principal representan los renglones
;;;                  	-las posiciones de los elementos de las sublistas representan las columnas
;;;						-los elementos de cada sublista representan el estado de cada casilla
;;;								*NIL - significa que la casilla se encuentra vacía
;;;								*X o O - significa que la casilla está ocupada, X corresponde a
;;;									     a la jugada de una persona y O a la jugada del agente jugador
;;;      Varible *output*:
;;;			El agente jugador debe entregar como respuesta un número del 1 al 16 (la posición en que
;;;			tirará su agente jugador), estos números representan las posiciones de casillas en 
;;;			el tablero y esta respuesta debe ser guardada en la variable *output*, no debe declarar
;;;			esta variable, sólo haga uso de ella.
;;;
;;;		Su agente recibirá el estado actual del tablero en cada jugada.
;;;============================================================================================================

;;; Estructura de los operadores
;;; ( < # > ( < X > < Y > ) )
;;;   < # >: Numero de operador.
;;;   < X >: Posicion x en el tablero (Columna).  
;;;   < Y >: Posicion y en el tablero (Fila). 

;;; Estructura del estado 
;;; ( < Tablero > < Player > )
;;;   < Tablero >: Tablero completo del juego.
;;;   < Player >: Jugador actual.

(defparameter *max-level* 6) ;;; Indicador del nivel máximo de profundidad a buscar.

;;; Operadores
(defparameter *operators* '(
  (1 (0 0))
  (2 (1 0))
  (3 (2 0))
  (4 (3 0))
  (5 (0 1))
  (6 (1 1))
  (7 (2 1))
  (8 (3 1))
  (9 (0 2))
  (10 (1 2))
  (11 (2 2))
  (12 (3 2))
  (13 (0 3))
  (14 (1 3))
  (15 (2 3))
  (16 (3 3))))

#| Predicado. isApplicable?
  
  De acuerdo al operador, verifica que la posicion proxima a insertar se encuentre disponible.

  @param tablero  - Tablero actual del juego.
  @param op       - Operador a verificar si es posible aplicarlo.
  @return         - T si la casilla marcada por el operador esta disponible (que su valor sea NIL).
|#
(defun isApplicable? (op tablero)
  (let (
    (x (first (second op)))
    (y (second (second op))))
      (null (nth x (nth y tablero)))))

#| Funcion. defOperations
  
  De acuerdo al tablero actual, determina todos los operadores posibles, es decir, indica las casillas
  disponibles (con valor NIL).

  @param tablero  - Tablero actual del juego.
  @return         - Lista de operadores posibles en el tablero actual.
|#
(defun defOperations (tablero)
  (let ((ops '()))
    (loop for op in *operators* do 
      (if (isApplicable? op tablero) 
        (push op ops)))
    ops))

#| Funcion. flip_player
  
  Cambia el simbolo del jugador actual, al del oponente.

  @param value    - Simbolo del jugador actual.
  @return         - Simbolo contrario al del jugador actual ('X' o 'O').
|#
(defun flip_player (value)
  (if (eql 'O value) 'X 'O))

#| Funcion. rowEvaluation
  
  Verifica si en la fila actual ha ganado un jugador.

  @param row      - Una fila del tablero actual.
  @return         - T si en toda la fila se encuentra el mismo simbolo de jugador.
|#
(defun rowEvaluation (row)
  (cond ((= 2 (length row)) (eql (first row) (second row)))
        (T (and 
          (eql (first row) (second row)) 
          (not (null (first row))) 
          (rowEvaluation (rest row))))))

#| Funcion. columnEvaluation
  
  Verifica si en la columna recibida ha ganado un jugador.

  @param column   - Columna a verificar del tablero actual.
  @param tablero  - Tablero actual de juego.
  @return         - T si en toda la columna se encuentra el mismo simbolo de jugador.
|#
(defun columnEvaluation (column tablero)
  (cond ((= 2 (length tablero)) (eql (nth column (first tablero)) (nth column (second tablero))))
        (T (and 
        (eql (nth column (first tablero)) (nth column (second tablero))) 
        (not (null (nth column (first tablero))))
        (columnEvaluation column (rest tablero))))))

#| Funcion. diagonalEvaluation
  
  Verifica si en una de las diagonales ha ganado un jugador.

  @param column   - Valora auxiliar de la columna actual a verificar.
  @param tablero  - Tablero actual de juego.
  @return         - T si en toda la diagonal se encuentra el mismo simbolo de jugador.
|#
(defun diagonalEvaluation (column tablero)
  (cond ((= 2 (length tablero)) 
          (eql (nth column (first tablero)) (nth (1+ column) (second tablero))))
        (T (and 
          (not (null (nth column (first tablero))))
          (eql (nth column (first tablero)) (nth (1+ column) (second tablero)))
          (diagonalEvaluation (1+ column) (rest tablero))))))

#| Predicado. isFinish?
  
  Indica si el movimiento realizado ha generado que uno de los jugadores gane.

  @param tablero  - Tablero actual de juego.
  @return         - T si en alguna fila, columna o diagonal ha ganado un jugador.
|#
(defun isFinish? (tablero)
  (let ((bandera NIL))
    (loop named cycle
      for row in tablero 
      for i from 0 below (length tablero)
      if bandera do (return-from cycle bandera) do
      (setq bandera (or
        (rowEvaluation row)
        (columnEvaluation i tablero))))
    
    (if (null bandera) 
      (setq bandera (and 
        bandera 
        (diagonalEvaluation 0 tablero) 
        (diagonalEvaluation 0 (reverse tablero))))
      bandera)))

#| Funcion. columnValue
  
  Indica con valor numerico las posibilidades de ganar en la columna actual.

  @param column   - Columna a verificar del tablero actual.
  @param tablero  - Tablero actual de juego.
  @param player   - Jugador actual.
  @return         - Valor numerico de las posibilidades de ganar, 0 si en alguna casilla
                    de la columna hay un simbolo del otro jugador.
|#
(defun columnValue (column tablero player)
  (let ((value 0))
    (loop for i from 0 below (length tablero) do
      (cond 
        ((and 
          (eql player (nth column (nth i tablero)))
          (not (null (nth column (nth i tablero))))) 
          (setq value (1+ value)))
        (T (return (setq value 0)))))
    value))

#| Funcion. rowValue
  
  Indica con valor numerico las posibilidades de ganar en la fila actual.

  @param row      - Fila a verificar del tablero actual.
  @param player   - Jugador actual.
  @return         - Valor numerico de las posibilidades de ganar, 0 si en alguna casilla
                    de la fila hay un simbolo del otro jugador.
|#
(defun rowValue (row player)
  (let ((value 0))
    (loop for column in row do 
      (cond 
        ((and 
          (eql player column)
          (not (null column))) 
          (setq value (1+ value))) 
        (T (return (setq value 0)))))
    value))

#| Funcion. columnValue
  
  Indica con valor numerico las posibilidades de ganar en una diagonal.

  @param tablero  - Tablero actual de juego.
  @param player   - Jugador actual.
  @return         - Valor numerico de las posibilidades de ganar, 0 si en alguna casilla
                    de la diagonal hay un simbolo del otro jugador.
|#
(defun diagonalValue (tablero player)
  (let (
    (value 0)
    (tam (length tablero)))
    (loop 
      for column from 0 below tam
      for row from 0 below tam do
      (cond 
        ((and 
          (eql player (nth column (nth row tablero)))
          (not (null (nth column (nth row tablero))))) 
        (setq value (1+ value)))
        (T (return (setq value 0)))))
    value))

#| Funcion. win_posibilities
  
  Indica con valor numerico las posibilidades de ganar en el tablero actual.

  @param tablero  - Tablero actual de juego.
  @param player   - Jugador actual.
  @return         - Valor numerico de las posibilidades de ganar con la ultima jugada realizada.
|#
(defun win_posibilities (tablero player)
  (let ((acum 0))
    (loop for i from 0 below (length tablero) do
      (setq acum (+ acum (rowValue (nth i tablero) player)))
      (setq acum (+ acum (columnValue i tablero player))))
    (setq acum (+ acum (diagonalValue tablero player)))
    (setq acum (+ acum (diagonalValue (reverse tablero) player)))
  acum))

#| Funcion. f 
  
  Funcion de evaluacion heuristica.

  @param estado   - Estado actual del sistema. 
  @return         - (Posibilidad de Ganar) - (Posibilidad de perder)
|#
(defun f (estado)
  (-
    (win_posibilities (first estado) (second estado)) 
    (win_posibilities (first estado) (flip_player (second estado)))))

#| Funcion. copyRow
  
  Crea una copia de una fila (del tablero).

  @param tablero  - Tablero actual de juego.
  @return         - Lista que representa la fila actual del tablero.
|#
(defun copyRow (row)
  (cond ((= 0 (length row)) NIL)
        (T (cons (first row) (copyRow (rest row))))))

#| Funcion. copySequential
  
  Crea una copia del tablero actual del juego fila por fila.

  @param tablero  - Tablero actual de juego.
  @return         - Lista que representa el tablero actual del juego 
                    (almacenado en otra direccion de memoria).
|#
(defun copySequential (tablero)
  (cond ((null (first tablero)) NIL)
        (T (cons (copyRow (first tablero)) (copySequential (rest tablero))))))

#| Funcion. apply-operator
  
  Aplica el operador al tablero actual del juego.

  @param estado   - Estado actual del juego.
  @param op       - Operador a aplicar en el estado actual del juego.
  @return         - Lista que representa el nuevo estado del juego.
|#
(defun apply-operator (estado op) 
  (let* (
    (x (first (second op)))
    (y (second (second op)))
    (player (second estado))
    (copia (copySequential (first estado))))
    (setf (nth x (nth y copia)) player)
    (list copia (flip_player player))))

#| Funcion. Negamax
  
  Algoritmo Negamax con poda alpha-beta usado para realizar la busqueda heuristica del juego del Gato.

  @param estado       - Estado actual del juego.
  @param profundidad  - Profundidad actual en la busqueda del algoritmo.
  @param alpha        - Valor alpha del estado actual. 
  @param beta         - Valor beta del estado actual.
  @return             - Lista que la integra el o uno de los valores "max", junto al movimiento asociado al valor.
|#
(defun Negamax (estado profundidad alpha beta)
  (let (
    (nvo_estado NIL)
    (ops NIL)
    (value NIL)
    (aux NIL))
    (cond 
      ((or (isFinish? (first estado)) (= profundidad 0))
        (if (> profundidad 0) (setq aux (* 10 (f estado))) (setq aux (f estado)))
        (format t "~%Alpha: ~3A~%Beta: ~3A~%" (first alpha) (first beta))
        (list aux NIL))
      (T 
        (setq ops (defOperations (first estado)))
        (loop named ciclo for op in ops do 
          (setq nvo_estado (apply-operator estado op)) 
          (setq value 
            (Negamax nvo_estado (1- profundidad) 
              (list (- (first beta)) (second beta)) 
              (list (- (first alpha)) (second alpha))))
          (setf (first value) (- (first value)))
          (if (>= (first value) (first beta))
            (return-from ciclo (setq alpha (list (first beta) (first op)))))
          (if (> (first value) (first alpha))
            (setq alpha (list (first value) (first op)))))
        alpha))))

#| Funcion. tictactoe

  Funcion solicitada por el problema que hace uso del algoritmo Negamax y el siguiente movimiento
  lo asocia a la variable global *output*.

  @param estado   - Tablero actual del juego.
  @return         - Valor asociado a *output* (mejor movimiento).
|#
(defun tictactoe (tablero)
  (let ((aux (Negamax (list tablero 'O) *max-level* (list most-negative-fixnum NIL) (list most-positive-fixnum NIL))))
    (setq *output* (second aux))))

;;;(defparameter desk '(
;;;  (NIL NIL NIL NIL)
;;;  (O   X   x   O)
;;;  (O   x   X   x)
;;;  (X   O   O   O)))

;;; (tictactoe desk)



