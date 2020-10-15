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
    (let ((pos (position-if #'oddp lista)))
        (cons (nth pos lista) pos)))

(primer-impar '(2 4 6 1 3 5)) ; => (1 . 3)
(primer-impar '("Hola" 4 9 2 "Mundo" (A B)))