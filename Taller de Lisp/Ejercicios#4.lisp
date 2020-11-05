#|
    CENTRO DE INVESTIGACION EN COMPUTACION
    ESCUELA SUPERIOR DE COMPUTO
    SEMESTRE B-20

ALUMNO: FERNANDO DANIEL RIVERA PAREDES
  FUNDAMENTOS DE INTELIGENCIA ARTIFICIAL
PROF.: DR. SALVADOR GODOY CALDERON

|#
;1 .- Devolver una lista donde se cimple el predicado del primer argumento
(defun Collect (predicado lista)
  (labels (
    (Collect-aux (predicado lista)
      (cond
        ((null lista) NIL)
        ((funcall predicado (first lista)) (cons (first lista) (collect-aux predicado (rest lista))))
        (T (collect-aux predicado (rest lista))))))
    (collect-aux predicado lista)))

(collect #'numberp '(1 "Hola" "Mundo" 1 50 9 1(A B C 1) D F 9 (43 2 1)))  ; => (1 1 50 9 1 9)
(collect #'listp '(1 "Hola" "Mundo" 1 50 9 1(A B C 1) D F 9 (43 2 1)))    ; => ((A B C 1) (43 2 1))
(collect #'symbolp '(1 "Hola" "Mundo" 1 50 9 1(A B C 1) D F 9 (43 2 1)))  ; => (D F)

;2 .- Verificar recursivamente si una lista es un palindromo
(defun palindromo (lista)
  (labels (
    (palindromo-aux (lista reversed-lista)
      (cond ((null lista) T)
            ((NOT (equal (first lista) (first reversed-lista))) NIL)
            (T (palindromo-aux (rest lista) (rest reversed-lista))))))
    (palindromo-aux lista (reverse lista))))

(palindromo '("R" "E" "C" "O" "N" "O" "C" "E" "R")) ; => T  
(palindromo '(1 2 3 4 5 () "Hola Mundo" (A B) (A B) "Hola Mundo" () 5 4 3 2 1)) ; => T
(palindromo '(1 "Hola" "Mundo" 1 50 9 1(A B C 1) D F 9 (43 2 1))) ; => NIL

;3 .- Convierte una cadena no palindromo en una que es palindromo de forma recursiva
(defun to-palindrome (str)
  (let ((list-str (coerce str 'list)))
    (labels (
      (convert (list-str reversed-list)
        (cond ((null reversed-list) list-str)
              (T  (convert (append list-str (list (first reversed-list))) (rest reversed-list))))))
      (setq list-str (convert list-str (reverse list-str))))
    (coerce list-str 'string)))

(to-palindrome "Reconozco") ; => "ReconozcooczonoceR"
(to-palindrome "ReconozceR") ; => "ReconozceRReczonoceR"
(to-palindrome "Anticonstitucionalmente") ; => "AnticonstitucionalmenteetnemlanoicutitsnocitnA"

;4 .- Lo mismo que la 3 pero de manera recursiva
(defun 2-palindrome (str)
  (let ((list-str (loop for c across str collect c)))
    (loop for i in (reverse list-str) do (push i (cdr (last list-str))))
    (coerce list-str 'string)))

(2-palindrome "Reconozco") ; => "ReconozcooczonoceR"
(2-palindrome "ReconozceR") ; => "ReconozceRReczonoceR"
(2-palindrome "Anticonstitucionalmente") ; => "AnticonstitucionalmenteetnemlanoicutitsnocitnA"

; 5 .- Rotar los elementos de una lista n veces
(defun ListRotate (lista n dir)
  (labels (
    (rotar (lista)
      (push (pop lista) (cdr (last lista)))
      lista)

    (list-rotate (lista n dir)
      (loop for i below n do 
        (cond ((equal dir ':right) (setq lista (reverse (rotar (reverse lista)))))
              ((equal dir ':left) (setq lista (rotar lista)))
              (T NIL)))
      lista))
    (list-rotate lista n dir)))

(ListRotate '(A B C D) 1 :left)

;6 .- Decuelve una lista de asociacion indicando el renglon del arreglo en el que se encuentra 
; el mayor valor de la columna correspondiente
(defun Max&Pos (A)
  (loop for index below (length A) collect 
    (cons (position (apply #'max x) x) index)))

(max&pos #2A((3 4 5) (6 7 8))) ; => ((15 18 21))
(max&pos '((5 -1) (1 0) (-2 3))) ; => ((1 8) (13 14))
(max&pos '((1 2 3) (3 4 5)))

;7 .- Duplicar la funcion reduce
(defun combine (func lista)
  (labels (
    (combine-aux (func lista)
      (cond ((null lista) (funcall func))
            ((null (rest lista)) (funcall func (first lista)))
            ((= 2 (length lista)) (funcall func (first lista) (second lista)))
            (T (funcall func (combine-aux func (reverse (rest (reverse lista)))) (first (last lista)))))))
    (combine-aux func lista)))

(combine #'* '(1 2 3 4 5)) ; => 120
(combine #'list '(1 2 3 4)); => (((1 2) 3) 4)
(combine (lambda (x y) (+ (* x 10) y)) '(1 2 3 4)); =>  1234

;8 .- Retorna el nivel del arbol donde se encuentra una cadena en una lista
(defun level (lista cadena)
  (let ((nivel 0))
    (labels(
      (recorrer-sublista (lista str nivel)
        (let ((valor (level-aux (first lista) str (1+ nivel))))
          (if (null valor)
            (level-aux  (rest lista) str nivel)
            valor)))
        
      (level-aux (lista str nivel)
        (cond ((null lista) NIL)
              ((equal (first lista) str) nivel)
              ((listp (first lista)) (recorrer-sublista lista str nivel))
              (T (level-aux (rest lista) str nivel)))))
      (level-aux lista cadena nivel))))

(level '(1 "Mundo" 1 50 9 1 (A B C) D ((F "Hola") 9) (43 2 1)) "Hola")        ; => 2 
(level '(1 2 3 4 5 "Hola Mundo" (A B) (A B) "Hola" "Mundo" 5 4 3 2 1) "Hola") ; => 0
(level '("R" "E" "C" "O" "N" "O" "C" "E" "R") "HOLA")                         ; => NIL

;9 .- Devuelve una lista de asociacion indicando la cantidad de ocurrencias de la letra en la cadena
(defun StrEncode (cadena)
  (let ((char-set ()) (lista-cadena (coerce (string-upcase cadena) 'list)))
    (loop for x in lista-cadena do
      (pushnew x char-set))
    (labels( 
      (strencode-aux (conj cadena)
        (cond ((null conj) NIL)
              (T (cons (cons (string (first conj)) (contador (first conj) cadena 0)) (strencode-aux (rest conj) cadena)))))

      (contador (ch cadena acum)
        (cond ((null cadena) acum)
              ((equal (first cadena) ch) (contador ch (rest cadena) (1+ acum)))
              (T (contador ch (rest cadena) acum)))))
      (reverse (strencode-aux char-set lista-cadena)))))

(strencode "Reconocer") 
(strencode "Hola Mundo")
(strencode "Anticonstitucionalmente") 

;10 .- Devuelve la cadena argumento cifrada de acuerdo a la otra cadena recibida
(defun StrCypher (cadena cryp)
  (setq cadena (string-downcase cadena))
  (let ((orig (coerce "abcdefghijklmnñopqrstuvwxyz" 'list)))
    (loop for i from 0 below (length cadena) do
      (setf (aref cadena i) (aref cryp (position (aref cadena i) orig))))
    cadena))

(StrCypher "HolaMundo" "bcdefghijklmnñopqrstuvwxyza") ; => "ipmbnvñep"

;11 .- Multiplicacion de matrices en tipo de dato array
(defun mult (A B)
  (if	(/= (second (array-dimensions A)) (first (array-dimensions B))) 
    "No se pueden multiplicar las matrices"
    (let* (
        (rows (first (array-dimensions A)))
        (columns (second (array-dimensions B)))
        (C (make-array (list rows columns))))
      (loop for i below rows do 
        (loop for j below columns do
          (setf 
            (aref C i j) 
            (loop for k below (second (array-dimensions A)) sum (* (aref A i k) (aref B k j))))))
      C)))
	

(mult #2A((1 2)) #2A((3 4 5) (6 7 8))) ; => ((15 18 21))
(mult #2A((1 2 3) (4 5 6)) #2A((5 -1) (1 0) (-2 3))) ; => ((1 8) (13 14))
(mult #2A((1 2)) #2A((3 4 5))) ; "No se pueden multiplicar las matrices"  (NIL)