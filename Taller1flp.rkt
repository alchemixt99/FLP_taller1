#lang eopl
;;Nombre: Jhon Erik Avila Ortiz
;;Codigo: 201210209
;;



;;************** Punto 2 **************
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
