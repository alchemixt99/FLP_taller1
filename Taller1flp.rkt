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

;; no se pudo hacer :(


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


;;************** Punto 8 ****************
;; Propósito:
;;   retorna una lista donde la posicion n-sima corresponde al resultado
;;   de aplicar la funcion f sobre los elementos en la posicion n-sima en lista1 y lista2. Puede asumir que
;;   f es una funcion que recibe dos argumentos y que ambas listan tienen igual tamaño

(define zip
  (lambda (f l1 l2)
    (let ((x (parse-exp l1)) (y (parse-exp l2)))
    (if (null? (unparse-exp x)) '()
        (cons (f (car (unparse-exp x)) (car (unparse-exp y)))(zip f (cdr (unparse-exp x))(cdr (unparse-exp y))))
        ))))

;;Pruebas
;;(zip + '(1 4) '(6 2))
;;(zip + '(11 5 6) '(10 9 8))

;;************** Punto 10 ****************
;; Propósito:
;;recibe una lista lista, una funcion f y un valor inicial acum. La
;;funcion f es una funcion que recibe dos parametros. Este procedimiento aplicara la funcion f a cada
;; elemento de la lista y a un valor acumulado inicialmente correspondiente al valor inicial acum.

(define foldl
  (lambda (ls f acum)
    (let((x (parse-exp ls)))
    (if (null? (unparse-exp x)) acum
        (foldl (cdr (unparse-exp x)) f (f acum (car (unparse-exp x))))
    )
  )
))

;;Prueba
;; (foldl '(12 4 21 11 8) + 0)
;; (foldl '(19 12 34 8 14 45) max 0)

;;************** Punto 11 ****************
;; Propósito:
;;recibe dos numeros a y b, una función binaria f, un valor inicial acum y una funcion unitaria filter
;;Este procedimiento aplicara la función f a cada valor entre a y b y a un valor acumulado que inicialmente 
;;corresponde al valor inicial de acum.

(define filter-acum
  (lambda (a b f acum filter)
    (if (<= a b) 
    (if (filter a)
         (filter-acum (+ a 1) b f (f acum a) filter)
         (filter-acum (+ a 1) b f acum filter)
    )
    acum)))
        
;;Prueba
;;(filter-acum 1 10 + 0 odd?)
;;(filter-acum 1 10 + 0 even?)

;;************** Punto 12 ****************
;; Propósito:
;;     recibe una lista con valores aleatorios y los ordena segun sea su operador < >
#|(define sort-aux
  (lambda ()
    
(define sort
  (lambda (ls f)
    (if (null? (cadr ls)) ls
        (
|#

;; Pruebas 
;;(sort '(8 2 5 2 3) <)
;;(sort '(8 2 5 2 3) >)

;;************** Punto 13 ****************
;; Propósito:
#|que toma un entero num y un arbol binario de busqueda que contiene el entero
num y retorna una lista de lefts y rights indicando como encontrar el nodo que contiene el entero num
en el arbol. Si num es encontrado en la raız, el procedimiento retorna una lista vacia.|#


(define path
  (lambda (num arbol)
    (if (null? arbol)
        (reportar-no-encontrado num)
        (if (eqv? num (car arbol))
            '()
            (let ( (nodo-izq (cadr arbol))
                   (nodo-der (caddr arbol)) )
              
              (if (< num (car arbol))
                  (cons 'left (path num nodo-izq))
                  (cons 'right (path num nodo-der))
                  )
              )
            )
        )
    )
  )
(define reportar-no-encontrado
  (lambda (num)
    (eopl:error "numero no encontrado")))
;; Pruebas
#|
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))
|#


;;************** Punto 15 ****************
;; Procedimiento:
#|que recibe dos arboles n-arios y retorna el subarbol
comun (estructuralmente). El subarbol comun se compone de las ramas 
que existen en ambos arboles. En este ejercicio el dato existente 
en cada nodo es irrelevante por lo que puede asumir que siempre sera 0.|#

(define common-subtree
  (lambda (arbol1 arbol2)
    (if (or (null? arbol1) (null? arbol2) )
          '()
          (let ( (nodo-izq1 (cadr arbol1))
                 (nodo-izq2 (cadr arbol2))
                 (nodo-der1 (caddr arbol1))
                 (nodo-der2 (caddr arbol2)) )
            
            (list (car arbol2)(common-subtree nodo-izq1 nodo-izq2)
                              (common-subtree nodo-der1 nodo-der2))
            )
          )
    )
  )
;; Pruebas
#|(common-subtree '(0 (0 () (0 () ()) )
                    (0 (0 () ()) (0 () ()) ))
                '(0 (0 (0 () ()) () )
                    (0 () (0 () ()) )))|#

;;************** Punto 16 ****************
;; Propósito:
;;     procedimiento que retorna la fila N del triangulo de Pascal.

(define suma-lst-hasta-n
  (lambda (lst-n n)
    (if (= 1 n)
        lst-n
        (let ( (lst1 (car (0-inserta-0 lst-n)))
               (lst2 (cadr (0-inserta-0 lst-n))) )
          
          (suma-lst-hasta-n (suma lst1 lst2) (- n 1))
))))

(define suma
  (lambda (lst1 lst2)
    (if (null? lst1)
        '()
        (cons (+ (car lst1) (car lst2)) (suma (cdr lst1) (cdr lst2)))
)))

(define 0-inserta-0
  (lambda (lst)
    (list (cons 0 lst) (append lst '(0)))
    ))

(define pascal
  (lambda (n)
    (let ((lst-n '(1)))
      (if (= n 1)
          lst-n
          (suma-lst-hasta-n lst-n n)
))))

;; Pruebas
;;(pascal 1)
;;(pascal 5)


(define max-lista
  (lambda (y)
    (let ((ls(parse-exp y)))
  (if (null? (unparse-exp ls)) ; edge case: empty list
      '()             ; return a special value signaling error   
      (let x ((ls (cdr (unparse-exp ls)))   ; rest of the list
                 (max (car (unparse-exp ls))))  ; assumed maximum
        (cond
          ((null? (unparse-exp ls)) max)    ; if the list is empty, return max
          ((> (car (unparse-exp ls)) max)(x (cdr (unparse-exp ls)) (car (unparse-exp ls)))) ; found new max
          (else (x (cdr (unparse-exp ls)) max))))))))

;;(max-lista '())

(define maximo
  (lambda (ls)
    (let((x (parse-exp ls)))
      (if (null? (unparse-exp x))
          '()
          ((> (car (unparse-exp ls)) max)(x (cdr (unparse-exp ls)) (car (unparse-exp ls))))
          ))))

(define (getlargest a_list)
  (if (null? a_list) ; edge case: empty list
      #f             ; return a special value signaling error   
      (let x ((a_list (cdr a_list))   ; rest of the list
                 (max (car a_list)))  ; assumed maximum
        (cond ((null? a_list) max)    ; if the list is empty, return max
              ((> (car a_list) max)   ; current element > max
               (x (cdr a_list) (car a_list))) ; found new max
              (else                      ; otherwise
               (x (cdr a_list) max)))))) 

;;(getlargest '(3 4 5))

(define my-max
  (lambda (l)
    (let ((x (parse-exp l)))
    (cond
      [(null? (unparse-exp x)) max]
      [(> (car (unparse-exp x)) max) (my-max (cdr (unparse-exp x)) (car (unparse-exp x)))]
      [else (my-max (cdr (unparse-exp x)) max)])
  (if (null? (unparse-exp x)) empty (my-max (unparse-exp x) (car (unparse-exp x)))))))