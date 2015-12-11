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

;;función auxiliar que nos permita construir las listas de factoriales
(define aux-lista_facts
  (lambda (act ant num)
    
    (if (eqv? act (+ num 1))
        '()
        (cons (* act ant) (aux-lista_facts (+ act 1)(* act ant) num))
        )
      )
    ) 


;;función principal
(define list-facts
  (lambda (num)
      (if (eqv? 0 num)
          '(1)
          (aux-lista_facts 1 1 num)))) 

;;Pruebas
;;(list-facts 5)
;;(list-facts 0)
;;(list-facts 1)

;;Pruebas
;;(exist? number? '(a b c 3 e))
;;(exist? boolean? '(a b c 3 e))

;;************** Punto 4 ****************
;;<valor> := <entero>
;;        := (lista-enteros)
;;
;;list-facts : n -> lista-enteros
;;
;;   Propósito:
;;       Este procedimiento  que recibe como par ́ametro un entero n y retorna una lista incremental de factoriales,
;;       comenzando en 1! hasta n!

;; PARA EL FIN DE SEMANA


;;************** Punto 5 ****************
;;<valor> := <lista>
;;        := entero
;;
;;inversions : lista-enteros -> n
;;
;;   Propósito:
;;       Este procedimiento  determina el numero de inversiones de lista.

(define fn_aux_inv
  (lambda (num lst)
    (let ((l-v (parse-exp lst)))
    (if(null? (unparse-exp l-v))
       0
       (if (> num (car (unparse-exp l-v)))
           (+ 1 (fn_aux_inv num (cdr (unparse-exp l-v))))
           (+ 0 (fn_aux_inv num (cdr (unparse-exp l-v))))
       )
    )
      )
  )
)

(define inversions
  (lambda (ls)
    (let ((list-values (parse-exp ls)))
      (if (null? (unparse-exp list-values))
          0
          (+ (fn_aux_inv (car (unparse-exp list-values)) (cdr (unparse-exp list-values))) (inversions (cdr (unparse-exp list-values))))
      )
    )
  )
  )

;; Pruebas 
;; (inversions '(2 3 8 6 1))
;; (inversions '(1 2 3 4))
;; (inversions '(3 2 1))


;;************** Punto 6 ****************
;;<valor> := <lista>
;;        := <lista>
;;
;;up : lista-var-exp -> lista-var-exp
;;
;;   Propósito:
;;       Este procedimiento remueve un par de parentesis de cada elemento del nivel más alto de lista

(define up
  (lambda (lst)
    (let ((list-val (parse-exp lst)))
    (if (null? (unparse-exp list-val))
        '()
        (if (pair? (car (unparse-exp list-val)))
            (append (car (unparse-exp list-val)) (up (cdr (unparse-exp list-val))) )
            (cons (car (unparse-exp list-val)) (up (cdr (unparse-exp list-val))) )
        )
    )
      )
))

;;Pruebas
;;(up '((1 2) (3 4)))
;;(up '((x (y)) z))

;;************** Punto 7 ****************
;;<valor> := <lista> <lista>
;;        := <lista>
;;
;;merge : lista-var-exp  lista-var-exp -> lista-var-exp
;;
;;   Propósito:
;;       Este procedimiento donde lista1 y lista2 son listas de enteros ordenadas ascendentemente. 
;;       El procedimiento merge retorna una lista ordenada de todos los elementos en lista1 lista2

(define merge 
(lambda (l1 l2)
  (let ((lst1 (parse-exp l1)) (lst2 (parse-exp l2)))
  (if (null? (unparse-exp lst1))
      (unparse-exp lst2)
      (if (null? (unparse-exp lst2))
          (unparse-exp lst1)
          (if (<= (car (unparse-exp lst1)) (car (unparse-exp lst2)))
              (cons (car (unparse-exp lst1)) (merge (cdr (unparse-exp lst1)) (unparse-exp lst2)))
              (cons (car (unparse-exp lst2)) (merge (unparse-exp lst1) (cdr (unparse-exp lst2))))           
          )
      )
  )
    )
 )
)
;;Pruebas
;;(merge '(1 4) '(1 2 8))
;;(merge '(35 62 81 90 91) '(3 83 85 90))