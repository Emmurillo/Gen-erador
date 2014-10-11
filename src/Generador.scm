(load "Archivos.scm")
(load "Operadores.scm")
(load "Listas.scm")

(define prob-hoja 0.85)

;OPERACIONES DE ARBOLES

; Genera un individuo de mínimo 1 de altura
(define crea-individuo
  (lambda ()
    (list (operador-random)
          (crea-individuo-aux)
          (crea-individuo-aux))))

; Genera un individuo aleatoriamente
(define crea-individuo-aux
  (lambda ()
    (cond ((< (random) prob-hoja) (crea-hoja))
          (else
           (list (operador-random)
                 (crea-individuo-aux)
                 (crea-individuo-aux)
                 )))))

;Determina el valor de una hoja
(define crea-hoja
  (lambda ()
    (cond ((< (random) 0.15) 'x)
          ((< (random) 0.30) 'y)
          (else
           (numero-random)))))

;Crea la población de tamaño cantidad
(define crea-poblacion
  (lambda (cantidad)
    (cond ((= cantidad 0) '())
          (else (cons (crea-individuo) (crea-poblacion (- cantidad 1) ) )))))

;Dados x y y pasa por parámetros los valores y evalua en el individuo
(define mostrar-individuo
  (lambda (individuo x y)
    (display individuo) (newline)
    (display ((eval (cons 'lambda(cons '(x y) (list individuo)))) x y))
    (display (newline))))

;Dados x y y pasa por parámetros los valores y evalua en el individuo
(define eval-individuo
  (lambda (individuo x y)
    ((eval (cons 'lambda(cons '(x y) (list individuo)))) x y)))

;Obtiene los z del individuo
(define z-individuo
  (lambda (indiv)
    (z-individuo-aux indiv (obtener-x-y))))

(define z-individuo-aux
  (lambda (indiv lista)
    (cond ((null? lista) '())
          (else
           (cons (eval-individuo indiv (caar lista) (cadar lista)) (z-individuo-aux indiv (cdr lista)))))))

;CRUCE
;Función que cruza con la raiz de indiv1, hijo derecho de indiv1 e hijo izquierdo de indiv2
(define cruce
  (lambda (ind1 ind2)
    (cond ((< (random) 0.7) (mutar 
                              (list (raiz-random ind1 ind2)
                                    (cadr ind1)
                                    (caddr ind2))))
          (else
           (list (raiz-random ind1 ind2)
                                    (cadr ind1)
                                    (caddr ind2))))))


;Función de mutación
(define mutar
  (lambda (ind)
    (cond ((number? ind) (arbol-pequeño))
          ((equal? ind 'x) (arbol-pequeño))
          ((equal? ind 'y) (arbol-pequeño))
          (else 
           (list (car ind)
                 (mutar (cadr ind))
                 (caddr ind))))))

(define arbol-pequeño
  (lambda ()
    (list (operador-random)
          (numero-random)
          (numero-random))))
      

;A partir de dos individuos retorna una de las 2 raíces
(define raiz-random
  (lambda (ind1 ind2)
    (cond ((< (random) 0.5) (car ind1))
          (else
           (car ind2)))))
           
;Muestra el  efecto del cruce
(define muestra-cruce
  (lambda (ind1 ind2)
    (display ind1)(newline)
    (display ind2)(newline)
    (display (cruce ind1 ind2))))

;FITNESS
;Determina la diferencia entre dos listas
(define dif-z
  (lambda (Z-Ind Z-Id)
    (cond ((null? Z-Ind) '())
          (else (cons (absoluto(- (car Z-Ind) (car Z-Id))) (dif-z (cdr Z-Ind) (cdr Z-Id)))))))

;Obtiene el fitness de un individuo
(define suma-diferencias
  (lambda (lista)
    (apply + lista)))

(define fitness-ind
  (lambda (individuo lista-z)
    (suma-diferencias (dif-z (z-individuo individuo) lista-z))))

;Fitness de la población
(define fitness-pob
  (lambda (pob lista-z)
    (cond ((null? pob) '())
          ((= (fitness-ind (car pob) lista-z) 0) (newline) (ganador (car pob)) (error "lambda encontrado") (list (fitness-ind (car pob) lista-z)))
          (else (cons (fitness-ind (car pob) lista-z) (fitness-pob (cdr pob) lista-z))))))

(define TP
  (lambda (lista)
    (apply + lista)))

(define TF-lista
  (lambda (TP lista)
    (map (lambda (x) (/ TP x)) lista)))

(define TF
  (lambda (TP lista)
    (apply + (map (lambda (x) (/ TP x)) lista))))

(define TK-lista
  (lambda (TF TF-lista)
    (map (lambda (x) (/ x TF)) TF-lista)))

(define fitness
  (lambda (pob lista-z)
    (fitness-aux (fitness-pob pob lista-z))))

(define fitness-aux
  (lambda (lista)
    (TK-lista (TF (TP lista) lista) (TF-lista (TP lista) lista))))

;Mostrar ganador
(define ganador
  (lambda (funcion)
    (display "(lambda (x y) ")
    (display funcion)
    (display ")")
    (newline)))

(define solucionar
  (lambda (lista-z)
    (fitness (crea-poblacion 40) lista-z)
    (solucionar lista-z)))

(define muestra-fitness
  (lambda (lista)
    (cond ((null? lista) '())
          (else
           (cons (exact->inexact (car lista)) (muestra-fitness (cdr lista)))))))