#|===================================================================================================================================
  MetaGato.lisp
      Juego de la variacion del Gato tradicional (tictactoe). Llamado 'Ultimate tictactoe' o 'Meta Gato' o 'Gato infinito'

      Función tictactoe: 
        Funcion principal del juego, solo se tiene que mandar a llamar desde consola
        para iniciar el juego. Este no recibe ningun argumento, automaticamente inicia la partida.
      
      Reglas del juego:
        * El jugador humano siempre inicia
        * El jugador humano puede elegir las casillas correspondientes marcadas como subtablero en la misma consola.
        * El mismo juego turna el juego entre la IA y el jugador humano.
        * Gana quien complete un tres en linea en el tablero mayor, ganando un marcador quien haya ganado en un subtablero,
          o bien si al siguiente jugador (IA o Humano) no le es posible tirar en el subtablero que le toca.

      Representacion del tablero general (global o completo): 
        | 01 02 03| 04 05 06| 07 08 09|
        | 10 11 12| 13 14 15| 16 17 18|
        | 19 20 21| 22 23 24| 25 26 27|
        | -  -  - + -  -  - + -  -  - |
        | 28 29 30| 31 32 33| 34 35 36|
        | 37 38 39| 40 41 42| 43 44 45|
        | 46 47 48| 49 50 51| 52 53 54|
        | -  -  - + -  -  - + -  -  - |
        | 55 56 57| 58 59 60| 61 62 63|
        | 64 65 66| 67 68 69| 70 71 72|
        | 73 74 75| 76 77 78| 79 80 81|

      Representacion del tablero mayor (Generalizacion entre subtableros del tablero global):
        | -  -  - |
        | -  -  - |
        | -  -  - |

      Representacion de un subtablero donde es posible tirar:  
        | 01 02 03|
        | 10 11 12|
        | 19 20 21|

======================================================================================================================================|#
;;; Estructura de los operadores
;;; ( < # > )
;;;   < # >: Numero de casilla donde es posible tirar en el subtablero.

;;; Estructura del estado 
;;; ( < Tablero > < Player > < Tablero_Mayor> <# Subtablero>)
;;;   < Tablero >:        Tablero completo del juego.
;;;   < Player >:         Jugador actual.
;;;   < Tablero_Mayor >:  Tablero mayor del juego (Generalizado).
;;;   < # Subtablero >:   Siguiente subtablero donde es posible elegir una casilla.

;;; Matriz que guarda todos los posibles valores de inicio de cada subtablero del juego.
(defparameter *init_values_table* '(
  (0  3   6)
  (27 30  33)
  (54 57  60)))

;;; Variable global donde se maneja todo el juego.
(defparameter *tablero* '())

;;; Variable global donde se mantiene el tablero mayor (Generalizacion del tablero completo).
(defparameter *tablero_mayor* '())

;;; Indicador del nivel máximo de profundidad a buscar.
(defparameter *max-level* 2) 

#| Funcion. color-text
  
  Imprime una cadena en cierto color especificado.

  @param cadena - Cadena de caracteres a imprimir de color.
  @param color  - Color especificado en el que se desea implantar sobre la cadena.
|#
(defun color-text (cadena color)
  (let((color
    (cond
      ((string= color "red") "31")
      ((string= color "green") "32")
      ((string= color "yellow") "33")
      ((string= color "white") "37")
      ((string= color "bright blue") "94")
      ((string= color "bright yellow") "93")
      ((string= color "bright cyan") "96")
      ((string= color "bright magneta") "95")
      (t "90"))))  
      (format t  (concatenate 'string "~c[" color "m" cadena "~c[0m") #\ESC #\ESC)))

#| Funcion. imprimirTablero
  
  Imprime el tablero completo del juego. De manera como se estructura el juego 'Ultimate tictactoe'

  @param tablero - Tablero completo del juego donde se cambiaron los valores nulos por el numero de casilla correspondiente.
|#
(defun imprimirTablero (tablero)
  (let ((acumColumn 0) (acumRow 1))
    (loop for row in tablero for j from 0 below (length tablero) do
      (loop for column in row for i from 1 to (length tablero) do
          (if (eql 0 (mod acumColumn 3))
            (color-text "|" "green"))
          (cond 
            ((null column) 
              (color-text (concatenate 'string (if (= j 0) " 0" " ") (write-to-string (+ (* 9 j) i))) "bright yellow"))
            ((eql 'X column) (color-text " X " "bright cyan"))
            (T (color-text " O " "bright magneta")))
          (incf acumColumn))
      (setq acumColumn 0)
      (color-text "|" "green")
      (format t "~%")
      (if (and (eql 0 (mod acumRow 3)) (< acumRow 7))
        (progn 
          (color-text "| -  -  - + -  -  - + -  -  - |" "green")
          (format t "~%")))
      (incf acumRow))))

#| Funcion. imprimirSubTablero
  
  Imprime un subtablero perteneciente al tablero general del juego. (Utilizado generalmente para imprimir el tablero mayor y 
  subtableros pertenecientes al tablero general)

  @param tablero - Subtablero del juego.
|#
(defun imprimirSubtablero (tablero)
  (loop for y in tablero do 
    (color-text "|" "green")
    (loop for x in y do 
      (cond
        ((eql x 'X) 
          (color-text " X " "bright cyan"))
        ((eql x 'O) 
          (color-text " O " "bright magneta"))
        ((numberp x) 
          (color-text (concatenate 'string (if (< x 10) " 0" " ") (write-to-string x)) "bright yellow"))
        (T 
          (color-text (concatenate 'string " " "- ") "bright yellow"))))    
    (color-text "|" "green")
    (format t "~%")))

#| Funcion. getSubtablero
  
  Genera una matriz aparentando el subtablero actual del juego. Cambiando los valores NIL por su numero de casilla correspondiente.

  @param tab  - Numero de subtablero a representar.
  @return     - Matriz que representa el subtablero actual.
|#
(defun getSubtablero (tab)
  (let* (
    (Y (floor (/ tab 3)))
    (X (mod tab 3))
    (X_mayor (* 3 X))
    (Y_mayor (* 3 Y))
    (init_value (+ (* 27 Y) X_mayor 1))
    (aux NIL))
    
    (list 
      (list 
        (if (null (setq aux (nth X_mayor (nth Y_mayor *tablero*)))) init_value aux)
        (if (null (setq aux (nth (1+ X_mayor) (nth Y_mayor *tablero*)))) (1+ init_value) aux)
        (if (null (setq aux (nth (+ X_mayor 2) (nth Y_mayor *tablero*)))) (+ 2 init_value) aux))
      (list 
        (if (null (setq aux (nth X_mayor (nth (1+ Y_mayor) *tablero*)))) (+ init_value 9) aux)
        (if (null (setq aux (nth (1+ X_mayor) (nth (1+ Y_mayor) *tablero*)))) (+ init_value 10) aux)
        (if (null (setq aux (nth (+ X_mayor 2) (nth (1+ Y_mayor) *tablero*)))) (+ init_value 11) aux))
      (list 
        (if (null (setq aux (nth X_mayor (nth (+ Y_mayor 2) *tablero*)))) (+ init_value 18) aux)
        (if (null (setq aux (nth (1+ X_mayor) (nth (+ Y_mayor 2) *tablero*)))) (+ init_value 19) aux)
        (if (null (setq aux (nth (+ X_mayor 2) (nth (+ Y_mayor 2) *tablero*)))) (+ init_value 20) aux)))))      

#| Funcion. getSubtableroValue
  
  Devuelve el numero de subtablero donde se encuentra la casilla recibida por parametro.

  @param casilla_actual - Tablero completo del juego.
  @return               - Numero de subtablero donde pertenece la casilla_actual
|#
(defun getSubtableroValue (casilla_actual)
  (let* (
    (X (floor (/ (mod casilla_actual 9) 3)))
    (Y (floor (/ casilla_actual 27)))
    (init_value (- casilla_actual (nth X (nth Y *init_values_table*)))))
    (cond ((> init_value 15) (- init_value 12))
          ((> init_value 5) (- init_value 6))
          (T init_value))))

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

#| Funcion. isWinOnSubtab?
  
  Verifica si en el subtablero actual hay un ganador, de ser asi, lo escribe en el tablero_mayor, si y solo si, no ha ganado 
  el otro jugador en ese mismo subtablero.

  @param tab            - Tablero completo del juego.
  @param player         - Jugador actual.
  @param tablero_mayor  - Tablero mayor (general) del juego.
|#
(defun isWinOnSubtab? (tab player tablero_mayor)
  (let* (
    (Y (floor (/ tab 3)))
    (X (mod tab 3))
    (pos (nth X (nth Y tablero_mayor))))

    (if (and 
      (null pos) 
      (isFinish? (getSubtablero tab)))
      (setf (nth X (nth Y tablero_mayor)) player))))

#| Predicado. isApplicable?
  
  De acuerdo al operador, verifica que la posicion proxima a insertar se encuentre disponible.

  @param op       - Operador (casilla) a verificar si es posible aplicarlo (es un numero).
  @return         - T si la casilla marcada por el operador esta disponible (que su valor sea NIL).
|#
(defun isApplicable? (op)
  (numberp op))

#| Funcion. defOperations
  
  De acuerdo al tablero actual, determina todas las casillas posibles del subtablero actual (con valor siendo un numero).

  @param estado   - Estado actual del juego.
  @return         - Lista de operadores (casillas disponibles) posibles en el tablero actual.
|#
(defun defOperations (estado)
  (let (
    (ops '())
    (subtablero (getSubtablero (fourth estado))))

    (loop for row in subtablero do 
      (loop for element in row do 
        (if (isApplicable? element) 
          (push element ops))))
    ops))

#| Funcion. flip_player
  
  Cambia el simbolo del jugador actual, al del oponente.

  @param value    - Simbolo del jugador actual.
  @return         - Simbolo contrario al del jugador actual ('X' o 'O').
|#
(defun flip_player (value)
  (if (eql 'O value) 'X 'O))

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
  @return         - Posibilidad de Ganar (generado aleatoriamente, con razon del 10 al 15% de las partidas totales de ganar)
|#
(defun f (estado)
  (setq estado estado)
  (random 400))

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
  (let (
    (X (mod (1- op) 9))
    (Y (floor (/ (1- op) 9)))
    (player (second estado))
    (copia_tablero (copySequential (first estado)))
    (copia_tablero_mayor (copySequential (third estado)))
    (subtablero_actual (getSubtableroValue (1- op))))

    (setf (nth X (nth Y copia_tablero)) player)
    (isWinOnSubtab? subtablero_actual player copia_tablero_mayor)
    (list copia_tablero (flip_player player) copia_tablero_mayor subtablero_actual)))

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
      ((or (isFinish? (third estado)) (= profundidad 0))
        (if (> profundidad 0) (setq aux (* 10 (f estado))) (setq aux (f estado)))
        (list aux NIL))
      (T 
        (setq ops (defOperations estado))
        (loop named ciclo for op in ops do 
          (setq nvo_estado (apply-operator estado op)) 
          (setq value 
            (Negamax nvo_estado (1- profundidad) 
              (list (- (first beta)) (second beta)) 
              (list (- (first alpha)) (second alpha))))
          (setf (first value) (- (first value)))
          (if (>= (first value) (first beta))
            (return-from ciclo (setq alpha (list (first beta) op))))
          (if (> (first value) (first alpha))
            (setq alpha (list (first value) op))))
        alpha))))

#| Funcion. turnHuman
  
  Funcion que generaliza todos los pasos para que el jugador humano haga su siguiente movimiento.

  @param estado           - Estado actual del juego.
  @param casilla_anterior - Casilla donde se realizo la jugada anterior.
  @return                 - Casilla donde se realizo la jugada actual.
|#
(defun turnHuman (estado casilla_anterior)
  (let (
    (tablero_anterior (getSubtableroValue casilla_anterior))
    (casilla NIL))
    (format t "La IA tiro en la casilla: ~2A~%~%Tablero hasta el momento:~%" (1+ casilla_anterior))
    (loop do
      (imprimirTablero *tablero*)
      (format t "~%Tablero Mayor: ~%")
      (imprimirSubtablero *tablero_mayor*)
      (format t "~%")
      (imprimirSubtablero (getSubtablero (fourth estado)))
      (princ "Elija una posicion del subtablero donde quiera tirar: ")
      (terpri)
      (setq casilla (read))
      (cond (
        (or 
          (not (integerp casilla)) (<= casilla 0) (>= casilla 82)
          (not (null (nth (mod (1- casilla) 9) (nth (floor (/ (1- casilla) 9)) *tablero*))))) 
        (format t "Valor no valido. Intente nuevamente.~%Tablero hasta el momento:~%~%")) 
        (T 
          (setq casilla (1- casilla))
          (setf (nth (mod casilla 9) (nth (floor (/ casilla 9)) *tablero*)) 'X)
          (return))))
    (setf (fourth estado) (getSubtableroValue casilla))
    (isWinOnSubtab? tablero_anterior 'X *tablero_mayor*)
    casilla))

#| Funcion. turnHuman
  
  Funcion que generaliza todos los pasos para que la IA haga su siguiente movimiento.

  @param estado           - Estado actual del juego.
  @param casilla_anterior - Casilla donde se realizo la jugada anterior.
  @return                 - Casilla donde se realizo la jugada actual.
|#
(defun turnIA (estado casilla_anterior)
  (let (
    (tablero_anterior (getSubtableroValue casilla_anterior))
    (casilla NIL))
    (format t "El jugador tiro en la casilla: ~2A~%~%Tablero hasta el momento:~%" (1+ casilla_anterior))
    (loop do
      (imprimirTablero *tablero*)
      (format t "~%Tablero Mayor: ~%")
      (imprimirSubtablero *tablero_mayor*)
      (format t "~%")
      (imprimirSubtablero (getSubtablero (fourth estado)))
      (setq casilla (second (Negamax estado *max-level* (list most-negative-fixnum NIL) (list most-positive-fixnum NIL))))
      (cond (
        (or 
          (not (integerp casilla)) (<= casilla 0) (>= casilla 82)
          (not (null (nth (mod (1- casilla) 9) (nth (floor (/ (1- casilla) 9)) *tablero*))))) 
        (format t "Valor no valido. Intente nuevamente.~%Tablero hasta el momento:~%~%")) 
        (T 
          (setq casilla (1- casilla))
          (setf (nth (mod casilla 9) (nth (floor (/ casilla 9)) *tablero*)) 'O)
          (return))))
    (setf (fourth estado) (getSubtableroValue casilla))
    (isWinOnSubtab? tablero_anterior 'O *tablero_mayor*)
    casilla))

#| Predicado. isFullTab?
  
  Determina si el tablero_mayor global se encuentra lleno.

  @return - T si y solo si todas las posiciones del tablero_mayor se encuentran ocupadas.
|#
(defun isFullTab? ()
  (let ((bandera T))
    (loop named cycle for row in *tablero_mayor* do
      (loop for elem in row do 
        (if (null elem) (return-from cycle (setq bandera NIL)))))
    bandera))

#| Funcion. whoIsWinner?
  
  Determina quien fue el ganador de la partida o bien, hubo un empate.

  @param player - Ultimo jugador en realizar su jugada.
  @return       - NIL si hubo un empate, el mismo jugador en caso contrario.
|#
(defun whoIsWinner? (player)
  (if (isFullTab?) NIL player))

#| Funcion. isFullSubtab?
  
  Indica si es posible relizar una jugada en el subtablero elegido.

  @param tab  - Numero de subtablero actual.
  @return     - T si es posible elegir una casilla en el subtablero, NIL en el caso contrario.
|#
(defun isFullSubtab? (tab)
  (let ((bandera T))
    (loop named cycle for row in (getSubtablero tab) do
      (loop for elem in row do 
        (if (numberp elem) (return-from cycle (setq bandera NIL)))))
    bandera))

#| Funcion. reset-globals
  
  Reinicia a sus valores iniciales el tablero completo y el tablero mayor.

|#
(defun reset-globals ()
  (defparameter *tablero* '(
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

  (defparameter *tablero_mayor* '(
    (NIL  NIL   NIL)
    (NIL  NIL   NIL)
    (NIL  NIL   NIL))))

#| Funcion. tictactoe
  
  Funcion principal para el desarrollo del juego. Reinicia las variables globales que cambian y va turnando entre jugador e IA.

|#
(defun tictactoe ()
  (reset-globals)
  (let (
    (edo (list *tablero* 'O *tablero_mayor* NIL))
    (casilla NIL)
    (jugador NIL)
    (bandera NIL)
    (winner "El ganador es: "))
    
    ;;; Tirada inicial hecho por el humano
    (loop do
      (imprimirTablero *tablero*)
      (princ "Elija una posicion del tablero donde quiera iniciar: ")
      (terpri)
      (setq casilla (read))
      (cond (
        (or 
          (not (integerp casilla)) (<= casilla 0) (>= casilla 82)
          (not (null (nth (mod (1- casilla) 9) (nth (floor (/ (1- casilla) 9)) *tablero*))))) 
        (format t "Valor no valido. Intente nuevamente.~%")) 
        (T 
          (setq casilla (1- casilla))
          (setf (nth (mod casilla 9) (nth (floor (/ casilla 9)) *tablero*)) 'X)
          (return))))
    (setf (fourth edo) (getSubtableroValue casilla))
    (loop do
      ;;; Tirada IA
      (setq jugador 'O)
      (setq casilla (turnIA edo casilla))
      (if (or 
        (isFinish? *tablero_mayor*) 
        (isFullSubtab? (getSubtableroValue casilla))) 
        (return))
      ;;; Tirada humano
      (setq jugador 'X)
      (setq casilla (turnHuman edo casilla))
      (if (or 
        (isFinish? *tablero_mayor*) 
        (isFullSubtab? (getSubtableroValue casilla))) 
        (return)))

    (setq bandera (whoIsWinner? jugador))
    (cond 
      ((eql bandera 'X) (setq winner (concatenate 'string winner "Human :3")))
      ((eql bandera 'O) (setq winner (concatenate 'string winner "IA B)")))
      (T (setq winner "Empate :/")))
    
    (format t "~A~%Tablero final:~%" winner)
    (imprimirTablero *tablero*)))















