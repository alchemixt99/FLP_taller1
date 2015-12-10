#lang eopl
;;Jhon Erik Avila Ortiz | 201210209
;;Sebastian Salazar     | 200938596
;;Taller 1

;;************** Punto 1 **************
;;<lista-de-elementos> := ()
;;list-tails : lista -> lista
;;
;;   Propósito:
;;       Este procedimiento recibe una lista y con esta construye una nueva usando 
;;       las sublistas de elementos consecutivos en la lista inicial.
;;
(define-datatype lista lista?
  (empty-list)
  (non-empty-list (first exp?)
                   (rest lista?))
  )

(define-datatype exp exp?
  (symbol-exp (sym symbol?))
  (number-exp (num number?))
  (lista-exp (lst lista?))
  (boolean-exp (bool boolean?)))


(define parse-exp
  (lambda (dato)
    (cond
      ((symbol? dato) (symbol-exp dato))
      ((number? dato) (number-exp dato))
      ((lista? dato) (lista-exp dato))
      ((boolean? dato) (boolean-exp dato))
      ((null? dato)(empty-list))
      ((pair? dato)
             (non-empty-list (parse-exp(if(null?(car dato))
                                           (empty-list)
                                           (if(pair?(car dato))
                                                  (parse-exp(car dato))
                                                  (car dato))))
             (parse-exp (cdr dato)))
             )
      ("Error en la sintaxis"))))

(define unparse-exp
  (lambda (expr)
    (cases lista expr
      (empty-list () '())
      (non-empty-list (first rest)
             (cons
              (cases exp first
                (symbol-exp  (symbol) symbol)
                (number-exp (number) number)
                (boolean-exp (boolean) boolean)
                (lista-exp (lista) (unparse-exp lista) )
                )
              (unparse-exp rest))))))
      
(define list-tails 
  (lambda (ls)
    (let ((y (parse-exp ls)))
      (if (null? (unparse-exp y))
          '()
          (cons (unparse-exp y)(list-tails(cdr (unparse-exp y))))
          )
      )
    )
  )

 
;;Pruebas
;;(list-tails '(1 2 3 4 5))
;;(list-tails '(1 a (e 4) 5 v))

;;************** Punto 2 **************
;;<expression> := boolean
;; exist : pred lista -> boolean
;;
;;   Propósito:
;;       Este procedimiento recibe como parametros un predicado (number? symbol? bool? list?) y retorna
;;       #t si al menos un elemento de la lista satisface el predicado

(define exist?
  (lambda (pred lst)
    (let ((ls (parse-exp lst)))
    (if (null? (unparse-exp ls))
        #f
        (if (pred (car (unparse-exp ls)))
            #t
            (exist? pred (cdr (unparse-exp ls)))
        )
    )
      )
    )
  )

;;Pruebas
;;(exist? number? '(a b c 3 e))
;;(exist? boolean? '(a b c 3 e))

;;************** Punto 3 ****************
;;<valor> := <entero>
;;        := (lista-enteros)
;;
;;list-facts : n -> lista-enteros
;;
;;   Propósito:
;;       Este procedimiento  que recibe como par ́ametro un entero n y retorna una lista incremental de factoriales,
;;       comenzando en 1! hasta n!


