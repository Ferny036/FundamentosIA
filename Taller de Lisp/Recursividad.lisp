; Recursividad, mapeos y filtros

; Vision en terminos simples, "Conjuntos"
; Conjuntos, se pueden especificar:
; Por extencion     A = {a,b,c,d,e}
; Por intencion     C = { x | 0 <= x <= 3.141592}
; Por induccion     Combinacion de las 2 anteriores....

; Una funcion se llama REcursiva si y solo si calcula o construye un conjunto inductivo
; Se conoce una funcion recursiva dada las siguientes situaciones
;   Se invoca a si misma
;   Invoca a otra funcion, que a su vez, invoca a la primera

; Ejemplo
(defun factorial (x)
    (cond   ((= x 1) 1)
            (T (* x (factorial (- x 1))))))

; Ejemplo  LISP "(Length )"
; Caso base:
;   Lista vacia -> 0
; Induccion:
;   (x ..) -> 1 + Conteo(...)
(defun num-elementos (lista)
    (cond   ( (null lista) 0)
            ( T (+ 1 (num-elementos (rest lista))) )))

;Ultima celda de una lista
; Caso base:
;   Lista vacia -> NIL
;   Lista con 1 elemento -> primer elemento
; Induccion:
;   (x y ...) -> (y ...)
(defun ultimo (lista)
    (cond   ((null lista) NIL)
            ((null (rest lista)) lista)
            (T (ultimo (rest lista)))))

; Reverse, invertir una lista
; Caso base:
;   Lista vacia -> ()
;   Lista con 1 elemento -> (x)
; Induccion:
;   (x y ...) -> append ((y ...)^R , (x))
(defun invierte (lista)
    (cond   ((null lista) lista)
            ((null (rest lista)) lista)
            (T (append (invierte (rest lista)) (list (first lista)) ))))

; TRACE permite rastrear las invocaciones recursivas de una funcion
(trace invierte)
#| (invierte '(1 2 3 4))
0: (INVIERTE (1 2 3 4))
    1: (INVIERTE (2 3 4))
    2: (INVIERTE (3 4))
        3: (INVIERTE (4))
        3: INVIERTE returned (4)
    2: INVIERTE returned (4 3)
    1: INVIERTE returned (4 3 2)
0: INVIERTE returned (4 3 2 1)
(4 3 2 1) |#
(untrace invierte) ;=> T

; Optimizacion de funciones recursivas
(defun pega-listas (lista1 lista2)
    (cond   ((null lista1) lista2)
            (T (cons (first lista1) (pega-listas (rest lista1) lista2)))))

; Optimizado
(defun invierte (lista)
    (labels invierte-aux (lista acumulado)
        (cond ((null lista) acumulado)
            (T (invierte-aux (rest lista) (cons (first lista) acumulado))))
            (invierte-aux lista nil)))

; Optmizando la funcion de Fibonacci
(defun Fib (x)
    (Fib-aux 0 1))

(defun Fib-aux (x acum1 acum2)
    form)

; REcomendaciones
; 1) Cada proceso separalo en dos funciones
; 2) La funcion principal solo invoca a la aux y no es recursiva
; 3) La funcion auxiliar (recursiva) agrega un argumento acumulador por cada rama de recursion
; 4) En el caso base de la funcion auxiliar, se entrega como respuesta el valor acumulado en el parametro acumulador

; Mapeo
; Es una funcion aque aplica un mismo procesamiento a todos los elementos de varias listas y en las posiciones correspondientes
(mapcar <funcion> <lista1> <lista2> ...)
(mapcar #'+ '(1 2 3) '(7 8 9)) ; => (8 10 12)

; Expresiones Lambda
(lambda (<argumentos>) <cuerpo>)
(mapcar #'(lambda (x) (append x x)) '(a b c) )
(mapcar #'(lambda (x y z) (list z y x)) '(a b c) '(1 2 3) '(x y z))

; Filtros
; Es una funcion que examina los elementos de una lista, aplicando una prueba a cada uno y eliminando aquellos que pasan la prueba aplicada
(defun filtra-pares (lista)
    (cond ((null lista) nil)
            ((evenp (first lista)) (filtra-pares (rest lista)))
            (T (cons (first lista) (filtra-pares (rest lista))))))

(filtra-pares '(1 2 3 4 5 6 7 8 9))

(defun filtra-ceros (lista)
    (cond ((null lista) nil)
            ((zerop (first lista)) (filtra-ceros (rest lista)))
            (T (cons (first lista) (filtra-ceros (rest lista))))))

; FUNCALL se utilizara para generalizar la funcion anterior

(defun filter (lista funcion)
    (cond ((null lista) nil)
            ((funcall funcion (first lista)) (filter (rest lista) funcion))
            (T (cons (first lista) (filter (rest lista) funcion)))))

(filter '(1 2 3 4 5 6 7 8 9) #'evenp)