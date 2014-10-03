(define operadores '(+ - / *))

;ARCHIVOS

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

; OPERACIONES MATEMÁTICAS
;Determina el valor absoluto de un número
(define absoluto
  (lambda (num)
    (cond ((< num 0) (* -1 num))
          (num))))

;OPERACIONES LISTAS
;Remueve el n-ésimo elemento de la lista, donde el primer elemento es el índice 0
;Saca los z de la lista leída del archivo
(define drop
  (lambda (lista)
    (drop-aux lista 0 2)))

(define drop-aux
  (lambda (lista cont ind)
    (cond ((null? lista) lista)
          ((equal? cont ind) (drop-aux (cdr lista) 0 ind))
          (else
           (cons (car lista) (drop-aux (cdr lista) (+ 1 cont) ind))))))


;Empaqueta los 'x' y 'y'
(define pack
  (lambda (lista)
    (cond ((null? lista) lista)
          (else
           (append (list (cons (car lista) (list (cadr lista)))) (pack (cddr lista)))))))

;OPERACIONES DE ARBOLES
;Obtiene un operador random
(define operador-random 
  (lambda ()
    (list-ref operadores (random (length operadores)))))

; Genera un individuo aleatoriamente
(define crea-individuo
  (lambda ()
    (cond ((< (random) 0.49999) (crea-hoja))
          (else
           (list (operador-random)
                 (crea-individuo)
                 (crea-individuo)
                 )))))

;Determina el valor de una hoja
(define crea-hoja
  (lambda ()
    (cond ((< (random) 0.15) 'x)
          ((< (random) 0.30) 'y)
          (else
           (- 10 (random 20))))))
             

