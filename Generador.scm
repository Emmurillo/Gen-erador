; Recibe: in = Un puerto de lectura.
; Retorna: Una lista con todos los objetos leídos del puerto hasta que se lea un caracter EOF.
(define read-objects
  (lambda (in)
    (let ([object (read in)])
      (cond 
        ((eof-object? object) '())
        (else (cons object (read-objects in)))))))

; Recibe: path = Ruta del archivo.
; Retorna: Una lista con cada objeto del archivo.
(define read-file
  (lambda (path)
    (read-objects (open-input-file path))))