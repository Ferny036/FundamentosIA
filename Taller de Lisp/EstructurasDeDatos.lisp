; Una de las caracteristicas mas importantes de una funcion es si opera de forma destructiva o no
;Muchas funciones trabajan en parejas..
; No destructiva
; reverse, append, remove, cl-subst
; Destructiva
; nreverse
; nconc
; delete
; cl-nsubst
; sort
; cl-merge
; Modificacion universal
; La funcion SETF permite modificar destructivamente cualquier posicion en cualquier estructura de datos
(setf <posicion> <valor>)

(defparameter *lista* '(a b c d))
(setf (third *lista*) 'x)

; Listas como pilas y conjuntos
; Se incluye funciones para menejo de listas que representan una pila
(defparameter *pila* '())
(push 'B *pila*)
(push 'A *pila*)
(pop *pila*)
; Push y pop son funciones destructivas

; Conjuntos
; Existen funciones para el manejo de listas que representan conjuntos
(adjoin <elemento> <conjunto>) ; Agregar elemento a conjunto
(member <elemento> <conjunto>) ; Encontrar elemento en conjunto
(union <conjunto1> <conjunto2>); Union de conjuntos
(intersection <conjunto1> <>); Interseccion entre conjuntos
(set-difference);
(subsetp); Verifica si el conjun

; Lista de asociacion
; Una lista de asociacion es una lista que contiene pareja llave-valor
((uno . 1) (dos . 2) (tres . 3) (cuatro . 4) ...)
; Es una asociacion entre dos conjuntos distintos apareando o emparejando los elementos en la misma posicion
; Llaves = {uno, ods, tres, cuatro}
; Valores = {1, 2, 3, 4}

; Creacion
; Para crear listas de asociacion se usa pairlis
(pairlis <lista1> <lista2> [<lista-asociacion>])
(setq A '(uno dos tres cuatro))
(setq B '(1 2 3 4))
(pairlis A B)

;Si pairlis se le proporciona el tercer argumento, entonces agrega las parejas formadas a la lista indicada
(setq C '())

; Agregar
; Tambien se puede usar la funcion acons para agregar una asociacion a una lista de asociaciones
(acons <llave> <valor> <lista-asociacion>)
(acons 'nueve 9 C)

; Buscar
; Existen 4 funciones
; assoc, assoc-if, rassoc, rassoc-if
(assoc 'cuatro C)
(assoc-if <funcion> C)
(rassoc '4 C)
(rassoc-if #'oddp C)

; Registros
; Common Lisp permite la creacion de estructuras de registro
(defstruct <etiqueta> <campo1> <campo2> ....)
(defstruct persona nombre paterno materno)
make-<estiqueta>
<etiqueta> - p
<etiqueta> - <campo#1>
<etiqueta> - <campo#2>
<etiqueta> - <campo#3>
...
(defstruct persona nombre paterno materno)
(defparameter Yo (make-persona :nombre 'Fernando :paterno 'Rivera :materno 'Paredes))
(persona-p yo)
(persona-nombre yo)
(persona-materno yo)
(persona-paterno yo)

; Jerarquias
(defstruct persona nombre paterno materno)
(defstruct (Astronauta (:include persona)) tamaño-casco (bebida-favorita 'Tang))
(setq X1 (make-astronauta :nombre 'Yuri :paterno 'Gagarin :Tamaño-casco 17.5 :bebida-favorita 'Vodca))
(setq X2(make-astronauta :nombre 'Neil :paterno 'Alden :materno 'Armstrong :Tamaño-casco 15.5))

; Arreglos
; Common Lisp soposta arreglos multidimensionales, para crear un arreglo se usa make-array
(make-array '(5) )
(make-array '(2 2))
(make-array '(5) :initial-element 1)
(make-array '(2 4) :initial-contents '((0 1 2 3) (5 6 7 8)))

; Acceso
(aref ); Lectura.- Posicionador, señala alguna posicion dentro de un arreglo
(setf ); Escritura .- Escribe en alguna localidad bien espificada
(defvar *arreglo* (make-array '(3 4)))
(setf (aref *arreglo* 2 2) "Manzana")
(aref *arreglo* 2 2)
; Arreglos dinamicos
(setq A (make-array '(3 2) :adjustable T :initial-contents '((A B) (C D) (E F))))
(adjust-array A '(2 4))
(array-rank A) ; Dimensiones de A
(array-dimensions A) ; La capacidad de cada una de sus dimensiones de A
(array-total-size A) ; Cantidad de elementos totales

; Tablas hash
; Es una estructua para almacenas parejas llave-valor de forma eficiente, el tiempo de acceso a cada elemento es constante
; Solo se puede consultar la asociacion de forma directa (solo las llaves)
; El compilador no puede facilmente desplegar el coontenido de la tabla
(make-hash-table :test :size :rehash-size :rehash-threshold)
:test Funcion para comparar llaves #'eql #'eq #'equal ; eql
:size Tamaño inicial de la tabla;
:rehash-size Numero de elementos a agregar cuando se llana la tabla
:rehash-threshold Nivel de llenado requerido para agregar los nuevos elementos

(gethash <llave> <tabla>); => <valor> T
(remhash <llave> <tabla>)
