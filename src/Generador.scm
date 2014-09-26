; Obtiene lista de numeros del archivo
(define obtener-lista
  (lambda (in)
    (let ([buffer (read in)])
      (cond 
        ((eof-object? buffer) '())
        (else (cons buffer (obtener-lista in)))))))

; Objetos del archivo
(define leer-archivo
  (lambda (ruta)
    (obtener-lista (open-input-file ruta))))

;Determina el valor absoluto de un número
(define absoluto
  (lambda (num)
    (cond ((< num 0) (* -1 num))
          (num))))