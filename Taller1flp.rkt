#lang eopl
;;Nombre: Jhon Erik Avila Ortiz
;;Codigo: 201210209
;;


;;************** Punto 1 **************
;;<lista-de-elementos> := ()
;;list-tails : lista -> lista2
;;
;;   Propósito:
;;       Este procedimiento recibe una lista y con esta construye una nueva usando 
;;       las sublistas de elementos consecutivos en la lista inicial.
;;
(define list-tails
  (lambda (ls)
    (if (null? ls)
        '()
        (cons ls (list-tails (cdr ls)))
        )
    )
  )
;;Pruebas
;;(list-tails '(1 2 3 4 5 ))
;;(list-tails '(1 a (e 4) 5 v))

;;************** Punto 3 **************
;;<lista-de-elementos> := ()
;;list-facts: n -> lista
;;
;;   Propósito:
;;       Este procedimiento recibe como par parámetro un entero n y retorna una lista incremental de factoriales,
;;       comenzando en 1! hasta n!
;;
(define aux-lf
  (lambda (act ant n)
    (if (eqv? act (+ n 1))
        '()
        (cons (* act ant) (aux-lf (+ act 1)(* act ant) n))))) 


(define list-facts
  (lambda (n)
    (if (= 0 n)
        0
        (aux-lf 1 1 n)))) 

;;Pruebas
;;(list-facts 5)
