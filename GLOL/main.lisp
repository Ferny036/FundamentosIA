(load "./GLOL.lisp")
(defparameter *estado-inicial* (list '(1 1 1 1) '(0 0 0 0)))
(defparameter *estado-final* (list '(0 0 0 0) '(1 1 1 1)))

(blind-search *estado-inicial*  *estado-final*  :breath-first)
(blind-search *estado-inicial*  *estado-final*  :depth-first)