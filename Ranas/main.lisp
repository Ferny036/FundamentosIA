(load "./Ranas.lisp")
(defparameter *estado-inicial*  
  (list
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    NIL
    (make-rana :color :verde)
    (make-rana :color :verde)
    (make-rana :color :verde)))

(defparameter *estado-final*  
  (list
    (make-rana :color :verde)
    (make-rana :color :verde)
    (make-rana :color :verde)
    NIL
    (make-rana :color :cafe)
    (make-rana :color :cafe)
    (make-rana :color :cafe)))
    
(blind-search *estado-inicial*  *estado-final*  :breath-first)
(blind-search *estado-inicial*  *estado-final*  :depth-first)
