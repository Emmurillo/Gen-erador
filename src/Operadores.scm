(load "Matematica.scm")
(define operadores '(+ - dv * expte bin-and bin-or bin-xor))
(define nums '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 1 2 3 4 5 6 7 8 9 10))

;OPERACIONES
(define trunc
  (lambda (num)
    (inexact->exact (truncate num))))

(define dv
  (lambda (x y)
    (cond ((= 0 y) (/ x 1))
          ((not (real? x)) 1)
          (else (/ x y)))))

(define expte
  (lambda (x y)
    (cond ((= 0 x) 0)
          (else (expt (absoluto x) y)))))

(define bin-and
  (lambda (x y)
    (bitwise-and (trunc x) (trunc y))))

(define bin-or
  (lambda (x y)
    (bitwise-ior (trunc x) (trunc y))))

(define bin-xor
  (lambda (x y)
    (bitwise-xor (trunc x) (trunc y))))


;Obtiene un operador random
(define operador-random 
  (lambda ()
    (list-ref operadores (random (length operadores)))))

;Obtiene un operador random
(define numero-random 
  (lambda ()
    (list-ref nums (random (length nums)))))