(load "Archivos.scm")
(load "Operadores.scm")
(load "Listas.scm")

;OPERACIONES DE ARBOLES

; Genera un individuo aleatoriamente
(define crea-individuo
  (lambda ()
    (cond ((< (random) 0.6) (crea-hoja))
          (else
           (list (operador-random)
                 (crea-individuo)
                 (crea-individuo)
                 )))))

;Determina el valor de una hoja
(define crea-hoja
  (lambda ()
    (cond ((< (random) 0.15) 10)
          ((< (random) 0.30) 20)
          (else
           (- 10 (random 20))))))

;Crea la población de tamaño cantidad
(define crear-poblacion
  (lambda (cantidad)
    (cond ((= cantidad 0) '())
          (else (cons (crea-individuo) (crear-poblacion (- cantidad 1) ) )))))

             

