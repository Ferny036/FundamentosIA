;; Escribir en consola

(write-line "Hola mundo")
(format t "Hola mundo")
(print "Hola mundo")

;; Leer desde consola
(read)
(read-line)

;; Escribir en archivos

(open "path/of/file" :direction :output :if-exist :rename-and-delete)

;; Leer archivos
(load "archivo.lisp")
(load (compile-file "archivo.lisp")) ;; Carga el archivo compilado

