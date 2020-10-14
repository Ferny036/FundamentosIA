; Variables globales
(defconstant *constante* <valor>)    ; => No se puede cambiar su valor
(defvar *variable* <valor> )              ; => No cambia su valor si ya se encuentra definida
(defparameter *parametro* <valor>); => Puede cambiar su valor con cualquier operacion de asociacion

; Variables locales
; Se puede anidar con otros let
(let ([<simbolo> <valor>]*)
    ; Cuerpo del let
    <action>
    ...
    <action>)

(defun analiza (lista)
    (let ((total (length lista))
        (first-numeric (numberp (first lista)))
        (last-numeric (numberp (first (last lista)))))
    (list 'elementos total first-numeric last-numeric)))

; Funciones locales
(flet )
(labels )
; Ejemplo con labels que cuenta la cantidad de elementos
(defun conteos (lista)
    (labels ( (conteo-elementos (lista))
        (length lista)
    ))
    (format t "lista: ~a tiene ~a elementos ~%"
        lista
        (conteo-elementos lista)))

; Sentencias de control
; If-then, If-then-else
(defun mayor (x1 x2)
    (if (or (not (numberp x1)) (not (numberp x2)))
        NIL
        (if (>= x1 x2) x1 x2)
    )
)

; Se pueden definir bloques de instrucciones con la macro PROGN
(progn <expr1> <expr2> ...)

; Para definir bloques PRGN se usan
(when <predicado> <proc-TRUE>) ; Equivalente a If-then
(unless <predicado> <proc-FALSE>) ; Equivalente a IF-not-then

(cond
    (<cond1> <proc1>)
    (<cond2> <proc2>)
    (<cond3> <proc3>)
    (T <proc-otherwise>))

(defun enfatiza (x)
    (cond ( (equal (first x) 'buen) (cons 'excelente (rest x)) )
            ( (equal (first x) 'mal) (cons 'pesimo (rest x)) )
                (T x)))

; Estructura case
(case <variable>
    (key1 action1 action2 action3 ...)
    (key2 action1 action2 action3 ...)
    ...)

;Existe una condicional pero de tipos
(typecase <variable>
    (integer ...)
    (float ...)
    (string ....)
    ....)

; Estructuras de iteracion
(do ([<var> <inicio> <actualizacion>]*)
    ((<terminacion>) <respuesta>)
    <bloque>
)

(do ((n 0 (+ n 1))) ((= n 5) 'Agua)
    (print n))

(do ((k 10 (* k 10)))
    ((> k 10000) k)
    (print k))

(do ((i 1 (+ i 1)) (j 5 (* j 2)) (k 100 (/ k 10)) )
    ( (> i 5) k )

    (print i)
    (print j)
    (print k))

(defun gaussSum (max-value)
    (let ((total 0))
        (do ((renglon max-value (1- renglon)))
            ((= renglon 0) total)
            (setq total (+ renglon total))
        )
    )
)

; Estructura for iterativa numerico con inicio en 0
(dotimes (<var> <expr> <result>)
    (<forms>))

(let ((total 0) (limit 10))
    (dotimes (n (+ limit 1) total )
        (setq total (+ total n))
    )
)

; Estrunctura que itera sobre una lista
(dolist (<var> <list> <result>)
    (<forms>))

;Estructura favorita para estructuras Repeat-Until
(loop for i in '(1 2 3) do
    (print i))

(loop
    for i in '(1 2 3 20 100)
    minimize i)
