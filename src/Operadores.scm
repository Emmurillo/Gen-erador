(define operadores '(+ - / * expt bin-and bin-or bin-xor))
(define nums '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 1 2 3 4 5 6 7 8 9 10))

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

;Obtiene un operador random
(define numero-random 
  (lambda ()
    (list-ref nums (random (length nums)))))