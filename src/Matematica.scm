; Valor absoluto de un número
(define absoluto
  (lambda (num)
    (cond ((< num 0) (* -1 num))
          (else num))))