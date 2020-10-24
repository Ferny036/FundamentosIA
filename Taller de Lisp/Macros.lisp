; Macro
; Proporcionan un completo sistema de evaluacion en tiempo de ejecucion
; Quote selectivo
; Quote invertido (back quote)
; Evita la evaluacion de todos los elementos no precedidos oir una coma
(setq x 3)

(eval `(* ,x ,x)) ; Evita la evaluacion de elementos no precedudos por una coma, en caso contrario, si seran evaluados

(setq x 4 y 3.5)
'(+ x 10 y) ; => (+ x 10 y) .- No se evalua

`(+ x 10 y) ; => Mismo caso que el anterior por no usar la coma
`(+ ,x 10 ,y) ; => (+ 4 10 3.5) => Ahora si se evalua


(defmacro cuadrado (x) `(* ,x ,x))
(macroexpand '(cuadrado (expt 3 2)))

; Variables
; La capacidad complreta de evaluacion de expresiones esta disponible en tiempo de compilacion, por lo tanto si podemos usar variables locales al definir una macro

(defmacro cuadrado2 (x) `(let ((tmp) ,x) (* tmp tmp)))
(macroexpand '(cuadrado2 (expt 3 2)))

; Operador separador para argumentos de macros
; Operador @ se usa para acceder a todos los elementos de una lista
; Es un operador no-destructivo
(setq inicio '(a))
(setq resto '(B C D))
`(,inicio ,@resto)

; En cambio el operador "punto" (.)  es un  operador destructivo para acceder a los elementos de una lista
`(,@inicio ,@resto)
`(,.inicio ,@resto)
inicio