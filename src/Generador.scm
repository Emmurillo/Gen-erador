(load "Archivos.scm")
(load "Operadores.scm")
(load "Listas.scm")
(load "Matematica.scm")

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
    (cond ((< (random) 0.6) (crea-hoja))
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
           (- 10 (random 20))))))

;Crea la población de tamaño cantidad
(define crear-poblacion
  (lambda (cantidad)
    (cond ((= cantidad 0) '())
          (else (cons (crea-individuo) (crear-poblacion (- cantidad 1) ) )))))

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
    (display indiv)(newline)
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
    (list (raiz-random ind1 ind2)
          (cadr ind1)
          (caddr ind2))))

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

(define fitness
  (lambda (individuo)
    (suma-diferencias (dif-z (z-individuo individuo) (obtener-z)))))




