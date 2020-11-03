; 1 .- Determinar si un elemento esta en la posicion indicada de una lista
; Retorna T si se encuentra ese elemento, NIL caso contrario
(defun elemInPos (elem lista pos)
  (cond ((null lista) nil)
        ((AND (equal elem (first lista)) (= pos 0)) T)
        (T (elemInPos elem (rest lista) (1- pos)))))

(ElemInPos 'A '(B C A) 2)                               ; => T
(ElemInPos '(A B) '((A B) 56732.33489 "Hola mundo") 0)  ; => T
(ElemInPos 5 '("Hola" 5 "Mundo") 2)                     ; => NIL

; 2 .- Crea una copia de una lista  a partir de donde se encuentre un el primer elemento "elem"
(defun Inicio-en (lista elem)
  (cond ((null lista) nil)
        ((equal elem (first lista)) lista)
        (T (Inicio-en (rest lista) elem))))

(inicio-en '(2 3 4) 3)
; => (3 4)
(inicio-en '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1)) 9)
; => (9 (A B C) D F 9 (43 2 1))
(inicio-en '(H O L A M U N D O) 3)
; => NIL

; 3 .- Crea una copia de una lista a par de la ultima ocurrencia del elemento "elem"
(defun Termino-en (lista elem)
  (reverse (Inicio-en (reverse lista) elem)))

(termino-en '(2 3 4) 3)
; => (2 3)
(termino-en '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1)) 9)
; => ("Hola" "Mundo" 10 50 9 (A B C) D F 9)
(termino-en '(H O L A M U N D O) 3)
; => NIL

; 4 .- Crea una lista con el primer elemento de la lista original que es un numero impar y su posicion
(defun primer-impar (lista)
  (labels 
    ((primer-impar-aux (lista contador)
      (cond 
        ((null lista) NIL)
        ((and (numberp (first lista)) (oddp (first lista))) (list (first lista) contador))
        (T (primer-impar-aux (rest lista) (1+ contador))))))
    (primer-impar-aux lista 0)))

(primer-impar '(2 4 6 1 3 5)) ; => (1 3)
(primer-impar '("Hola" 4 9 2 "Mundo" (A B))) ;=> (9 2)
(primer-impar '("Hola" (M U N D O) 2 4 6)) ; => NIL

; 5 .- Devuelve una lista con el ultimo numero real mayor que cero y cuantas veces se encuentra en la lista

(defun ult-elem-real (lista)
  (labels 
    ((ult-elem-real-aux (lista)
      (cond ((null lista) NIL)
            ((and (numberp (first lista)) (realp (first lista)) (>= (first lista) 0)) (list (first lista) (count (first lista) lista)))
            (T (ult-elem-real-aux (rest lista))))))
    (ult-elem-real-aux (reverse lista))))

(ult-elem-real '(2 4 5 1 3 5)) ; => (5  2)
(ult-elem-real '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => (2  3)
(ult-elem-real '("Hola" M U N D O)) ; => NIL

; 6 .- Devuelve una celda con la cantidad de numeros reales y sublistas que se encuentran en la lista original

(defun conteo (lista)
	(labels 
    ((conteo-aux (lista numbers sublists)
      (cond ((null lista) (list numbers sublists))
            ((numberp (first lista)) (conteo-aux (rest lista) (1+ numbers) sublists))
            ((listp (first lista)) (conteo-aux (rest lista) numbers (1+ sublists)))
            (T (conteo-aux (rest lista) numbers sublists)))))
    (conteo-aux lista 0 0)
  ))

(conteo '(2 4 5 1 3 5)) ; => (6 0)
(conteo '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => (5 1)
(conteo '("Hola" (A B) (NIL) (M U N D O) (3 . 1) 5 8)) ; => (2 4)

; 7 .- Devuelve una lista con los elementos en un solo nivel de profundidad
(defun aplana (lista)
  (labels (
    (aplana-aux (lista)
      (cond ((atom lista) (list lista))
            (T (append (aplana-aux (car lista)) (if (cdr lista) (aplana-aux (cdr lista))))))))
  (aplana-aux lista)))


(aplana '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => (2 "Hola" 4 9 2 "Mundo" 2 A B)
(aplana '(((((A)))))) ; => (A)
(aplana '((A (B C) ((D (E)))) (F G) H))

; 8 .- Contiene m sublistas de m elementos, devolver una lista de los elementos de la diagonal de la matriz
(defun diagonal (matriz)
	(labels ( 
    (add-element (matriz acum i)
      (push (nth i (first matriz)) acum)
      (diagonal-aux (rest matriz) acum (1+ i)))

    (diagonal-aux (matriz acum i)
      (cond ((null matriz) (reverse acum))
            (T (add-element matriz acum i)))))
  (diagonal-aux matriz () 0)))

(diagonal '((1 2) (3 4))); => (1 4)
(diagonal '((1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 1))) ; (1 1 1 1 1)
(diagonal '((1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6))) ; (1 2 3 4 5 6)

; 9 .- Devuelve una lista de misma longitud, agregando A si es un atomo, L si es una lista y N si es un numero
; en su lugar correspondiente
(defun sustitucion-letras (lista)
	(labels (
    (add-element (lista elem acum)
      (push elem acum)
      (sust-letras-aux (rest lista) acum)
    )

    (sust-letras-aux (lista acum)
      (cond ((null lista) (reverse acum))
            ((listp (first lista)) (add-element lista 'L acum))
            ((numberp (first lista)) (add-element lista 'N acum))
            (T (add-element lista 'A acum)))))
    (sust-letras-aux lista ())))
  
(sustitucion-letras '((A B) 56732.33489 "Hola mundo")) ;(L N A)
(sustitucion-letras '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; (N A N N N A N L)
(sustitucion-letras '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1))) ; (A A N N N L A A N L)

; 10 .- Suma de los elementos numericos de una lista
(defun suma-numerica (lista)
	(labels (
    (suma-n-aux (lista acum)
      (cond ((null lista) acum)
            ((numberp (first lista)) (suma-n-aux (rest lista) (+ acum (first lista))))
            (T (suma-n-aux (rest lista) acum)))))
    (suma-n-aux lista 0)))

(suma-numerica '(1.0 2 3 4 5 6 7 8 9 10)); => 55.0
(suma-numerica '(2 "Hola" 4 9 2 "Mundo" 2 (A B))); => 19
(suma-numerica '("Hola" "Mundo" 10 50 9 (A B C) D F 9 (43 2 1))); => 78

; 11 .- Filtra-vocales, lista donde se han removido todas las vocales 
(defun filtra-vocales (lista)
  (labels (
    (recorrer-lista (funcion lista)
      (cons (funcall funcion (first lista)) (funcall funcion (rest lista))))

    (remove-vocal-string (str)
      (find str "aeiou" :test #'char-equal))

    (filtra-vocales-aux (lista)
      (cond ((null lista) NIL)
            ((listp (first lista)) (recorrer-lista #'filtra-vocales-aux lista))
            ((stringp (first lista)) (cons (remove-if #'remove-vocal-string (first lista)) (filtra-vocales-aux (rest lista))))
            ( (or (not (characterp (first lista))) (char/= (first lista) #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)) 
              (cons (first lista) (filtra-vocales-aux (rest lista))))
            (T (filtra-vocales-aux (rest lista))))))
    (filtra-vocales-aux lista)))

(filtra-vocales '("Anticonstitucionalmente"))       ; => ("ntcnsttcnlmnt")
(filtra-vocales '(2 "Hola" 4 9 2 "Mundo" 2 (A B)))  ; => (2 "Hl" 4 9 2 "Mnd" 2 (A B))
(filtra-vocales '((#\A #\U) 56732.33489 (#\Space ( #\e (#\a ((("Hola mundo") #\b)))) #\d #\e #\g) "Mejor no"))  ; => (NIL 56732.336 (#\  ((((("Hl mnd") #\b)))) #\d #\g) "Mjr n")

; 12 .- Filtra-multiplos, lista donde se han removido todos los multiplos del entero recibido
(defun filtra-numeros (lista num)
  (labels (
    (recorrer-lista (funcion lista num)
      (cons (funcall funcion (first lista) num) (funcall funcion (rest lista) num)))

    (filtra-numeros-aux (lista num)
      (cond ((null lista) NIL)
            ((listp (first lista)) (recorrer-lista #'filtra-numeros-aux lista num))
            ((and (numberp (first lista)) (= 0 (mod (first lista) num))) (filtra-numeros-aux (rest lista) num)) 
            (T (cons (first lista) (filtra-numeros-aux (rest lista) num))))))
    (filtra-numeros-aux lista num)))

(filtra-numeros '(3 6 4 8 9 12 16 15 18 20 21) 3) ;=> (4 8 16 20)
(filtra-numeros '(2 "Hola" 4 9 2 "Mundo" 2 (A B)) 2) ; => ("Hola" 9 "Mundo" (A B))
(filtra-numeros '(A ((3 ("PI" 6 #\a) (B 4 8 "PI" 9 (#\a C 12 16 #\a D))) (15 18 ("PI" (20 #\a) E) 21) "PI")) 3) ;=> (A ((("PI" #\a) (B 4 8 "PI" (#\a C 16 #\a D))) (("PI" (20 #\a) E)) "PI"))

; 13 .- Funcion que cuenta el numero de celdas internas de una lista
(defun celdas (lista)
  (labels (
    (recorrer-lista (funcion lista acum)
      (setq acum (funcall funcion (first lista) acum) )
      (funcall funcion (rest lista) acum))

    (celdas-aux (lista acum)
      (cond ((null lista) acum)
            ((listp (first lista)) (recorrer-lista #'celdas-aux lista acum))
            (T (celdas-aux (rest lista) (1+ acum))))))
    (celdas-aux lista 0)))

(celdas '(3 6 4 8 9 12 16 15 18 20 21)) ;=> 11
(celdas '(2 "Hola" 4 9 2 "Mundo" 2 (A B))) ; => 9
(celdas '(A ((3 ("PI" 6 #\a) (B 4 8 "PI" 9 (#\a C 12 16 #\a D))) (15 18 ("PI" (20 #\a) E) 21) "PI"))) ;=> 24

; 14 .- Construir una funcion con aridad indeterminada que implemente el operador logico de la implicacion logica
(defun implica (&rest p)
	(labels (
    (implica-aux (proposiciones)
      (cond 
        ((= (length proposiciones) 2) (OR (NOT (first proposiciones)) (second proposiciones)))
        (T (OR (NOT (implica-aux (remove (car (last proposiciones)) proposiciones :count 1 :from-end T))) (last proposiciones))))))
    (if (>= (length p) 3) (car (implica-aux p)) (implica-aux p))))

(implica T NIL) ; => NIL
(implica T T T) ; => T
(implica NIL NIL NIL NIL NIL) ; => NIL

; 15 .- Funcion que multiplica 2 matrices
(defun transpuesta (M)
	(let ((aux ()))
		(loop for _ below (length (car M)) do (push () aux))
		(loop for i below (length M) do
			(loop for j below (length (car M)) do
				(cond ((null (nth j aux)) (push (nth j (nth i M)) (nth j aux)))
							(T (push (nth j (nth i M)) (cdr (last (nth j aux))))))))
							aux))

(defun mult (A B &optional arg)
	(labels (
    (dot-product (vectorA vectorB) 
      (loop for x in vectorA for y in vectorB sum (* x y)))

    (operations (A B C)
      (let ((vectorA (first A)))
          (setq C (append C (list (loop for vectorB in B collect (dot-product vectorA vectorB))))))
      (mult-aux (rest A) B C))

    (mult-aux (A B &optional (C ()))
      (cond ((null (first A)) C)
            (T (operations A B C)))))
    
  (setq arg (if (/= (length (car A)) (length B)) "No se pueden multiplicar las matrices" (mult-aux A (transpuesta B))))))

(mult '((1 2)) '((3 4 5) (6 7 8))) ; => ((15 18 21))
(mult '((1 2 3) (4 5 6)) '((5 -1) (1 0) (-2 3))) ; => ((1 8) (13 14))
(mult '((1 2)) '((3 4 5))) ; "No se pueden multiplicar las matrices" 

; 16.- La funcion devuelve NIL si elem no es un elemento de lista, de lo contrario, devuelve la sublista que comienza con la primera instancia de elem
(defun Buscar (elem lista)
  (cond ((null lista) NIL)
        ((equal elem (first lista)) (nconc (if (listp elem) elem (list elem)) (rest lista)))
        (T (Buscar elem (rest lista)))))

(Buscar 'A '(B C D E A F G H)) ; => (A F G H)
(Buscar '(A B) '(12 53 "Hola mundo" C D (A B) A B))

; 17.- Funcion que recibe 2 elementos y una lista, cambia el elemento 1 por elemento 2

(defun cambia (lista orig sust)
  (labels (
    (cambia-aux (lista orig sust)
      (print lista)
      (cond ((null lista) NIL)
            ((equal (first lista) orig) (cambia-aux (rest lista) orig (setf (first lista) sust)))
            ((listp (first lista)) (progn (cambia-aux (first lista) orig sust) (cambia-aux (rest lista) orig sust)))
            (T (cambia-aux (rest lista) orig sust)))))
    (cambia-aux lista orig sust))
  lista)

(cambia '(B C D E A F G H) 'A 55) ; (B C D E 55 F G H)
(cambia '(12 53 "Hola mundo" ((A B)) C D (A B) A B) '(A B) 'PI) ;'(12 53 "Hola mundo" (PI) C D PI A B)
(cambia '(1 "Hola" "Mundo" 1 50 9 1(A B C 1) D F 9 (43 2 1)) 1 'POKEMON); (POKEMON "Hola" "Mundo" POKEMON 50 9 POKEMON (A B C POKEMON) D F 9 (43 2 POKEMON))

; 18.- Comparar con time el rendimiento de cada una de las funciones de Fibonacci

(time (fib 50))
#|
Evaluation took:
  272.981 seconds of real time
  271.859375 seconds of total run time (271.843750 user, 0.015625 system)
  99.59% CPU
  1,010,035,719,609 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "Naive recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (if (< n 2) n
      (+ (fib (1- n)) (fib (- n 2)))))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  4,958 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "Tail-recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  4,551 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "loop-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  4,329 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "do-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (do ((i n (1- i))
      (f1 0 f2)
      (f2 1 (+ f1 f2)))
      ((= i 0) f1)))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  5,365 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "CPS computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n k)
                    (if (zerop n)
                        (funcall k 0 1)
                      (fib-aux (1- n) (lambda (x y)
                                        (funcall k y (+ x y)))))))
          (fib-aux n #'(lambda (a b) a))))
  
#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  4,995 processor cycles
  0 bytes consed
|#
(defun fib (n)
  (labels ((fib2 (n)
                (cond ((= n 0)
                      (values 1 0))
                      (t
                      (multiple-value-bind (val prev-val)
                                            (fib2 (- n 1))
                          (values (+ val prev-val)
                                  val))))))
    (nth-value 0 (fib2 n))))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  6,105 processor cycles
  0 bytes consed
|#
(defun fib (n)
  "Successive squaring method from SICP"
  (check-type n (integer 0 *))
  (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                          ((evenp count)
                           (fib-aux a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
                          (t (fib-aux (+ (* b q) (* a q) (* a p))
                                      (+ (* b p) (* a q))
                                      p
                                      q
                                      (- count 1))))))
          (fib-aux 1 0 0 1 n)))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  9,731 processor cycles
  0 bytes consed
|#
(defun fib (n)
  (if (< n 2) n
    (if (oddp n) 
      (let ((k (/ (1+ n) 2)))
        (+ (expt (fib k) 2) (expt (fib (1- k)) 2)))
      (let* ((k (/ n 2)) (fk (fib k)))
        (* (+ (* 2 (fib (1- k))) fk) fk)))))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  3,589 processor cycles
  0 bytes consed
|#
(defun fib (n &optional (i 1) (previous-month 0) (this-month 1)) 
 (if (<= n i)
      this-month
    (fib n (+ 1 i) this-month (+ this-month previous-month))))
  
#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  4,921 processor cycles
  0 bytes consed
|#
(defun fast-fib-pair (n)
  "Returns f_n f_{n+1}."
  (case n
    ((0) (values 0 1))
    ((1) (values 1 1))
    (t (let ((m (floor n 2)))
         (multiple-value-bind (f_m f_m+1)
             (fast-fib-pair m)
           (let ((f_m^2   (* f_m f_m))
                 (f_m+1^2 (* f_m+1 f_m+1)))
             (if (evenp n)
                 (values (- (* 2 f_m+1^2)
                            (* 3 f_m^2)
                            (if (oddp m) -2 2))
                         (+ f_m^2 f_m+1^2))
                 (values (+ f_m^2 f_m+1^2)
                         (- (* 3 f_m+1^2)
                            (* 2 f_m^2)
                            (if (oddp m) -2 2))))))))))
                    
#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  273,837 processor cycles
  0 bytes consed
|#
(defun fib (n)
  (* (/ 1 (sqrt 5))
     (- (expt (/ (+ 1 (sqrt 5)) 2) n)
	(expt (/ (- 1 (sqrt 5)) 2) n))))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  10,471 processor cycles
  0 bytes consed
|#
(defun fib (n)
  (/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
        (expt (/ (- 1 (sqrt 5)) 2) n))
     (sqrt 5)))

#|
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  11,766 processor cycles
  0 bytes consed
|#
(defun fib (n)
  (prog1 (round (/ (expt (/ (+ 1 (sqrt 5)) 2) n) (sqrt 5)))))

; 19 .- Implementar la misma funcion que mapcar de CL
(defun transpuesta (M)
	(let ((aux ()))
		(loop for _ below (length (car M)) do (push () aux))
		(loop for i below (length M) do
			(loop for j below (length (car M)) do
				(cond ((null (nth j aux)) (push (nth j (nth i M)) (nth j aux)))
							(T (push (nth j (nth i M)) (cdr (last (nth j aux))))))))
							aux))

(defun mapea (funcion &rest listas)
  (labels (
    (mapea-aux (funcion listas)
      (cond ((null listas) NIL)
            (T (cons (apply funcion (first listas)) (mapea-aux funcion (rest listas)))))))
    (mapea-aux funcion (transpuesta listas))))

(mapea #'+ '(1) '(2) '(3)) ; => (6) 
(mapea (lambda (x) (+ x 10)) '(1 2 3 4)) ; => (11 12 13 14)
(mapea #'list '(123 symbol "string") '(1 2 3)); => ((123 1) (SYMBOL 2) ("string" 3))

; 20 .- Devuelve una lista con los elementos en un solo nivel de profundidad
(defun aplana (lista)
  (labels (
    (aplana-aux (lista)
      (cond ((atom lista) (list lista))
            (T (append (aplana-aux (car lista)) (if (cdr lista) (aplana-aux (cdr lista))))))))
  (aplana-aux lista)))

; 21 .- Elimina los elementos de una lista que no son numericos y numericos menores al numero brindado por parametro
(defun Elimina (lista num)
  
  (labels (
    (filtra-lista (lista num funcion)
      (cond 
        ((null lista) NIL)
        ((not (funcall funcion (first lista))) (filtra-lista (rest lista) num #'numberp))
        (T (filtra-numeros lista num #'>=))))

    (filtra-numeros (lista num funcion) 
      (cond 
        ((null lista) NIL)
        ((funcall funcion num (first lista)) (filtra-lista (rest lista) num #'numberp))
        (T (cons (first lista) (filtra-lista (rest lista) num #'numberp))))))
    (filtra-lista lista num #'numberp)))

(elimina '("Hola" M U N D O 2 4 6) 10)                        ; => NIL         
(elimina '(1 "Hola" "Mundo" 1 50 9 1 A B C 1 D F 9 43 2 1) 5) ; => (50 9 9 43)
(elimina '("Hola" "Mundo" 10 50 9 A B C D F 9 43 2 1) 1)      ; => (10 50 9 9 43 2)

; 22 .- Cambia un elemento en comun de dos listas anidadas
(defun pegaycambia (listaA listaB orig sust)
  (let ((lista (append listaA listaB)))
    (labels (
      (pegaycambia-aux (lista orig sust)
        (cond ((null lista) NIL)
              ((equal (first lista) orig) (pegaycambia-aux (rest lista) orig (setf (first lista) sust)))
              ((listp (first lista)) (progn (pegaycambia-aux (first lista) orig sust) (pegaycambia-aux (rest lista) orig sust)))
              (T (pegaycambia-aux (rest lista) orig sust)))))
      (pegaycambia-aux lista orig sust))
  lista))
  
(pegaycambia '(12 53 "Hola mundo" ((A B)) C D (A B) A B) '(B C D E A F G H) '(A B) 'PI) ; => (12 53 "Hola mundo" (PI) C D PI A B B C D E A F G H) 
(pegaycambia '(B C D E A F G H) '(12 53 "Hola mundo" ((A B)) C D (A B) A B) 'A 3.141592); => (12 53 "Hola mundo" ((3.141592 B)) C D (3.141592 B) 3.141592 B B C D E 3.141592 F G H)
(pegaycambia '(() ()) '(H O L A () (DE) () N U E V O) () "Hola Mundo") ("Hola Mundo" "Hola Mundo" H O L A "Hola Mundo" (DE) "Hola Mundo" N U E V O)

; 23 .- Quicksort
(defun q-sort (l &optional (f #'<))
  (if (null (cdr l)) l
    (append (q-sort (remove-if-not #'(lambda (x) (funcall f x (car l))) (cdr l)) f)
            (list (car l))
            (q-sort (remove-if #'(lambda (x) (funcall f x (car l))) (cdr l)) f))))

(let ((lista1 (loop for l from 1 to 10 collect (random 25)))
      (lista2 (loop for l from 1 to 20 collect (random 50)))
      (lista3 (loop for l from 1 to 100 collect (random 500))))
      (print lista1)
      (print (q-sort lista1))
      (terpri)
      (print lista2)
      (print (q-sort lista2))
      (terpri)
      (print lista3)
      (print (q-sort lista3))
      (terpri))
