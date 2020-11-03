; 1 .- Determinar si un elemento esta en la posicion indicada de una lista
; Retorna T si se encuentra ese elemento, NIL caso contrario
(defun ElemInPos (elem lista pos)
  (equal (nth pos lista)  elem))

(ElemInPos 'A '(B C A) 2)                               ; => T
(ElemInPos '(A B) '((A B) 56732.33489 "Hola mundo") 0)  ; => T
(ElemInPos 5 '("Hola" 5 "Mundo") 2)                     ; => NIL

; 2 .- Crea una copia de una lista  a partir de donde se encuentre un el primer elemento "elem"
(defun Inicio-en (lista elem)
  (member elem lista))

(inicio-en '(2 3 4) 3)
; => (3 4)
(inicio-en '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1)) 9)
; => (9 (A B C) D F 9 (43 2 1))
(inicio-en '(H O L A M U N D O) 3)
; => NIL

; 3 .- Crea una copia de una lista a par de la ultima ocurrencia del elemento "elem"
(defun Termino-en (lista elem)
  (reverse (member elem (reverse lista))))

(termino-en '(2 3 4) 3)
; => (2 3)
(termino-en '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1)) 9)
; => ("Hola" "Mundo" 10 50 9 (A B C) D F 9)
(termino-en '(H O L A M U N D O) 3)
; => NIL

; 4 .- Crea una lista con el primer elemento de la lista original que es un numero impar y su posicion
(defun primer-impar (lista)
	(loop for i in lista 
		if (and (numberp i) (oddp i)) 
			do (return (cons i (position i lista)))))

(primer-impar '(2 4 6 1 3 5)) ; => (1 . 3)
(primer-impar '("Hola" 4 9 2 "Mundo" (A B))) ;=> (9 . 2)
(primer-impar '("Hola" (M U N D O) 2 4 6)) ; NIL

; 5 .- Devuelve una lista con el ultimo numero real mayor que cero y cuantas veces se encuentra en la lista

(defun ult-elem-real (lista)
	(let ((elem nil))
		(loop for i in (reverse lista) 
			if (and (numberp i) (realp i) (>= i 0) (null elem)) 
				do (setq elem i) (return))
		(cond	((null elem) NIL)
					(T (cons elem (count elem lista))))))
    
(ult-elem-real '(2 4 5 1 3 5)) ; => (5 . 2)
(ult-elem-real '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => (2 . 3)
(ult-elem-real '("Hola" M U N D O)) ; NIL

; 6 .- Devuelve una celda con la cantidad de numeros reales y sublistas que se encuentran en la lista original

(defun conteo (lista)
	(let ((numbers 0) (sublists 0))
		(loop for i in lista 
			if (numberp i) do (setq numbers (1+ numbers))
			if (listp i) do  (setq sublists (1+ sublists)))
		(cons numbers sublists)))

(conteo '(2 4 5 1 3 5)) ; (6 . 0)
(conteo '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; (5 . 1)
(conteo '("Hola" (A B) (NIL) (M U N D O) (3 . 1) 5 8)) ; (2 . 4)

; 7 .- Devuelve una lista con los elementos en un solo nivel de profundidad
(defun aplana (lista)
	(let ((acum ()) (elem NIL))
		(loop named original when (null lista) return acum do 
			(loop named copy do
				(setf elem (pop lista))
				(cond	((listp elem) (return-from copy T))
							(T (setq acum (nconc acum (list elem))))))
			(dolist (sub-elem elem) (setq lista (nconc lista (list sub-elem)))))))

; Devuelve los elementos ordenados de acuerdo al nivel en el que se encontraban
(aplana '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => (2 "Hola" 4 9 2 "Mundo" 2 A B)
(aplana '(((((A)))))) ; => (A)
(aplana '((A (B C) ((D (E)))) (F G) H))

; 8 .- Contiene m sublistas de m elementos, devolver una lista de los elementos de la diagonal de la matriz
(defun diagonal (matriz)
	(let ((acum ()))
		(loop for i from 0 below (length matriz)
			do (push (nth i (nth i matriz)) acum))
		(reverse acum)))

(diagonal '((1 2) (3 4))); => (1 4)
(diagonal '((1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 1))) ; (1 1 1 1 1)
(diagonal '()) ; (1 2 3 4 5 6)

; 9 .- Devuelve una lista de misma longitud, agregando A si es un atomo, L si es una lista y N si es un numero
; en su lugar correspondiente
(defun sustitucion-letras (lista)
	(let ((acum ()))
		(loop for i in lista do 
			(cond	((numberp i) (push 'N acum))
						((listp i) (push 'L acum))
						(T (append acum (push 'A acum)))))
		(reverse acum)))

(sustitucion-letras '((A B) 56732.33489 "Hola mundo")) ;(L N A)
(sustitucion-letras '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; (N A N N N A N L)
(sustitucion-letras '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1))) ; (A A N N N L A A N L)

; 10 .- Suma de los elementos que solo son numeros
(defun suma-numerica (lista)
	(let ((acum 0))
		(loop for i in lista if (numberp i) do (setq acum (+ acum i)))
			acum))

(suma-numerica '(1.0 2 3 4 5 6 7 8 9 10)); => 55.0
(suma-numerica '(2 "Hola" 4 9 2 "Mundo" 2 (A B))); => 19
(suma-numerica '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1))); => 78

; 11 .- 

; 12 .- 

; 13 .-

; 14 .- Construir una funcion con aridad indeterminada que implemente el operador logico de la implicacion logica
(defun implica (&rest p)
	(cond
		((< (length p) 2) (print "Dos argumentos como minimo"))
		(T (let ((anterior T) (bandera T)) 
			(loop initially (setq anterior (pop p)) for actual in p do
				(setq anterior (OR (NOT anterior) actual))
				(setq bandera (equal (first p) actual)))
			(cond	
				((AND bandera (null (first p))) (NOT anterior))
				(T anterior))))))

(implica T T)
(implica )
(implica )

; 15 .- Funcion que multiplica 2 matrices
(defun transpuesta (M)
	(let ((aux ()))
		(loop for _ below (length (car M)) do (push () aux))
		(loop for i below (length M) do
			(loop for j below (length (car M)) do
				(cond ((null (nth j aux)) (push (nth j (nth i M)) (nth j aux)))
							(T (push (nth j (nth i M)) (cdr (last (nth j aux))))))))
							aux))

; Para facilitar el calculo hago uso de la funcion transpuesta
(defun mult (A B)
	(let ((C ()) (temp 0))
		(loop for _ below (length A) do (push () C))
		(setq B (transpuesta B))
		(cond	((/=  (length (car A)) (length (car B))) (print "No se pueden multiplicar las matrices"))
			(T 
				(loop for row-a in A for i below (length A) do 
					(loop for row-b in B do
						(loop for kA in row-a for kB in row-b do 
							(setq temp (+ temp (* kA kB))))
						(cond ((null (nth i C)) (push temp (nth i C)))
									(T (push temp (cdr (last (nth i C))))))
						(setq temp 0)))))C))

(mult '((1 2)) '((3 4 5) (6 7 8))) ; => ((15 18 21))
(mult '((1 2 3) (4 5 6)) '((5 -1) (1 0) (-2 3))) ; => ((1 8) (13 14))
(mult '((1 2)) '((3 4 5))) ; "No se pueden multiplicar las matrices"  (NIL)
