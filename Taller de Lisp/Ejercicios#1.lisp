#|
    CENTRO DE INVESTIGACION EN COMPUTACION
    ESCUELA SUPERIOR DE COMPUTO
    SEMESTRE B-20

ALUMNO: FERNANDO DANIEL RIVERA PAREDES
  FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
PROF.: DR. SALVADOR GODOY CALDERON

|#

; 1.a .- Obtener el 5to elemento de la lista
(set 'X '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))
(nth 4 X)
; => (B C)

; 1.b .- Calcular el total de segundos de un a;o bisiesto
(* 366 24 60 60)
; => 31622400

;1.c .- Verificar si x != 0 & x<=y
(AND (/= x 0) (<= x y))

;1.d .- Lista con la solucion a
; 2x2 + 7x + 5 = 0
(list (/ (+ (sqrt (- (expt 7 2) (* 4 2 5))) -7) (* 2 2))
(/ (- (sqrt (- (expt 7 2) (* 4 2 5))) -7) (* 2 2)))
; => (-1.0 2.5)

; 2.a
(+ (* 2 4) (- 6 8))
; => 6

;2.b
(/ (+ 5 (- 4 3)) (+ 6 (/ 2 5)))
; => 15/16

; 2.c
(sqrt (/ (- 1.4502 (- -4 (/ 3 8))) (expt -1 (expt (- 3 5) (/ 1 3)))))
; => #C(7.355944 -11.196843)

;2.d
(expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))
; => #C(1.4500145 -0.065120235)

;3.a
(cdar '((one two) three four))
; => unmatched close parenthesis OR (TWO)

;3.b
(append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
; => ((EVA LISA) KARL SVEN EVA LISA KARL SVEN)

;3.c
(subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
; => (EVA GITAN LISA GITAN KARIN)

;3.d
(remove 'sven '(eva sven lisa sven anna))
; => (EVA LISA ANNA)

;3.e
(butlast '(karl adam nilsson gregg alisson vilma) 3)
; => (KARL ADAM NILSSON)

;3.f
(nth 2 '(a b c d e))
; => C

;3.g
(nthcdr 2 '(a b c d e))
; => (C D E)

;3.h
(intersection '(a b c) '(x b z c))
; => (C B)

;3.i
(cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))
; => (4)

;4 .- Recibir como parametro  ((A . x) (B . y) (C . z))
; retornar (((x y) . A) ((y z) . C) ((z y x) . B))
(defun Recombina (lista)
  (list
    (cons (list (cdr (first lista)) (cdr (second lista))) (car (first lista)))
    (cons (list (cdr (second lista)) (cdr (third lista))) (car (third lista)))
    (cons (list (cdr (third lista)) (cdr (second lista)) (cdr (first lista))) (car (second lista)))))

(set 'args (list (cons 'A 0) (cons 'B 1) (cons 'C 3)))
(Recombina args)
; => (((0 1) . A) ((1 2) . C) ((2 1 0) . B))

;5 .- Recibir un numero
; Retornar si es un numero real y diferente de cero

(AND (/= #C(1 5) 0.0) (realp #C(1 5))); => NIL
(AND (/= 1.5 0.0) (realp 1.5))        ; => T
(AND (/= 5 0.0) (realp 5))            ; => T

;6 .- Analiza si es un atomo, numero, lista, celda o lista vacia
; Retorna los valores de validacion (T, NIL)
(defun Analiza (X)
  (list (atom X) (numberp X) (listp X) (consp X) (eq () X)))

(analiza ()) ; => (T NIL T NIL)
(analiza 5.6784) ; => (T T NIL NIL NIL)
(analiza (list (cons 'A 0) (cons 'B 1) (cons 'C 3))) ; => (NIL NIL T T NIL)

;7 .- Intercalar elementos de 2 listas
; Retorna una lista con los elementos intercalados
(defun Intercala (A B)
  (set 'C ())
  (loop
  (if (null (first A))
    (push (pop A) C))
  (if (null (first B))
    (push (pop B) C))
  (when (AND (eql (first A) NIL) (eql (first B) NIL)) (return C)))
  (reverse C))

(intercala '(A B C) '(D E F))  ;=> (A D B E C F)
(intercala '() '(A B C))       ;=> (A B C)
(intercala '(A C E) '(B D F G));=> (A B C D E F G)

;8 .- Comprueba si los elementos de 2 listas son del mismo tipo
; Retorna T si todos sus elementos son del mismo tipo, NIL en caso contrario
(defun mismoTipo (listaA listaB)
  (let ((bandera T))
    (loop for x in listaA for y in listaB do  
      (if  (NOT (equal (type-of x) (type-of y))) (setq bandera NIL)))
      bandera))

(mismoTipo '(A 0.52 '(C B A D)) '(P 0.6718 '(P O P)))       ; => T
(mismoTipo '(A 0 (cons 'F (C B A D))) '(P 0.6718 '(P O P))) ; => NIL
(mismoTipo '(0.52 A '(C B A D)) '(P 0.6718 '(P O P)))       ; => NIL

;9 .- Comprueba si un string es un palindromo comparando entre mayusculas y minusculas
; Retorna T en caso de que sea un
(defun APalindromo (str)
    (set 'strwitoutspaces (remove #\Space str))
    (string= strwitoutspaces (reverse strwitoutspaces)))

(apalindromo "Anita lava la tinA") ;=> T
(apalindromo "Reconocer")          ;=> NIL
(apalindromo "HolaaloH")           ;=> T

;10 .- Indica si un año es bisiesto o no
(= 0 (mod year 4))