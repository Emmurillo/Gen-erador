(define operadores '(+ - / * expt bin-and bin-or bin-xor))

;OPERACIONES
(define bin-and
  (lambda (x y)
    (bitwise-and (numerator x) (numerator y))))

(define bin-or
  (lambda (x y)
    (bitwise-ior (numerator x) (numerator y))))

(define bin-xor
  (lambda (x y)
    (bitwise-xor (numerator x) (numerator y))))


;Obtiene un operador random
(define operador-random 
  (lambda ()
    (list-ref operadores (random (length operadores)))))