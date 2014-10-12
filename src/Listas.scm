;OPERACIONES LISTAS
;Remueve el n-ésimo elemento de la lista, donde el primer elemento es el índice 0
;Saca los z de la lista leída del archivo
(define drop
  (lambda (lista ind num)
    (drop-aux lista ind num)))

(define drop-aux
  (lambda (lista cont ind)
    (cond ((null? lista) lista)
          ((equal? cont -1) (drop-aux (cdr lista) 0 ind))
          ((equal? cont ind) (drop-aux (cdr lista) 0 ind))
          (else
           (cons (car lista) (drop-aux (cdr lista) (+ 1 cont) ind))))))


;Empaqueta los 'x' y 'y'
(define pack
  (lambda (lista)
    (cond ((null? lista) lista)
          (else
           (append (list (cons (car lista) (list (cadr lista)))) (pack (cddr lista)))))))


;Obtiene los X y Y empaquetados del archivo
(define obtener-x-y
  (lambda (ruta)
    (pack (drop (leer-archivo ruta) 0 2))))

;Obtiene los X y Y empaquetados del archivo
(define obtener-z
  (lambda (ruta)
    (drop (drop (leer-archivo ruta) -1 2) 1 1)))