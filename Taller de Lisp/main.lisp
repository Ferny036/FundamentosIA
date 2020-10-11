;; Carateres
#\A
#\@
#\\

;; Funciones
#'sqrt
#'+

;; Numeros complejos
#C(0 1) ;; 0 + 1i
#C(5 -2) ;; 5 - 2i

;; Arreglos
#2A((nil nil) (nil nil) (nil nil))

;; Registros
#S(Terna :uno nil :dos nil :tres nil)

;; Formatos en diferentes bases
#xFFF       ;; Hexadecimal
#o77        ;; Octal
#b1011    ;; Binario

;; Caso de NIL

(atom nil) ;; => T     Es un atomo
(listp nil) ;; => T     Es una lista vacia

;; Nil, () y '() son notaciones equivalentes
;; FIRST, REST y LAST de Nil se define como Nil

;; Predicados de igualdad
(eq a b)       ;; Mas especifico - Mas exigente - a y b existen en la misma direccion de memoria
(eql a b)      ;; a y b son iguales, el mismo tipo y mismo valor (atomos)
(equal a b)    ;; a y b son iguales, si son isomorfos (estructuras y listas)
(= a b)        ;; Similar a "equal", pero solo admite valores numericos
(equalp a b)   ;; Mas general - Menos exigentes - a y b son iguales en algun sentido de valor asociado

;; Asociacion
(set '<variable> <valor>)           ;; Solo asocia 1 valor a 1 variable
(setq <variable> <valor> ...)    ;; Asocia varios valores a varias variables

;; Funciones

;; No evalua sus argumentos
(defun <etiqueta> (<arg1> <arg2> <arg3> ...)
    <definicion>)

(defun promedio (x y) (/ (+ x y) 2.0))


(defun suma (x y)
    (* (+ x y) 2))

(setq suma '(a b c))

;; Funciones de aridad indefinida "Numero infdefinido de argumentos"
(+ a b c d ...);; a + b + c + d + ....
(+ a b c);; a + b + c
(+ a);; => a
(+) ;; => 0

(defun muestra (&rest argumentos) <body>)
;; Ejemplo
(defun primero-ultimo (&rest arg)
    (print (length arg))
    (print (first arg))
    (print (first (last arg))))

;; Argumentos opcionales y valores por omision directiva &optional
(defun muestra (&optional arg) <body>) ;; Sino esta asignado un valor por omision, a esta se le asigna NIL
(defun enlista (a b &optional c d) (list a b c d))

(defun agrega (&optional (lista-a '(a b c)) (b 20))
    (cons b lista-a))

;; Argumentos etiquetados directiva &key
;; Tambien los argumentos son opcionales
(defun muestra (&key args....) <body>)
(defun 1de4 (&key uno dos tres cuatro)
    (list uno dos tres cuatro))

;; Cambio de etiqueta, ejemplo
(defun fruta
    (&key ((:pera p) 1) ((:manzana m) 0) ((:uva u)) )
    (list p m u))
