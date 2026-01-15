(load-option 'synchronous-subprocess)

(define (sys/read fd n)
  (let ((c (peek-char)))
    (if (eof-object? c)
      ""
      (string (read-char)))))

(define (sys/write fd s n)
  (display s)) ; hack

(define sys/exit exit)

(define function? procedure?)

(define / quotient)

(define (string-length s) (bytevector-length (string->utf8 s)))

(define (string-list s)
  (map string (string->list s)))

(define runtime "s")

(define sys/close (lambda args (error "not implemented")))
(define sys/dup (lambda args (error "not implemented")))
(define sys/dup2 (lambda args (error "not implemented")))
(define sys/execve (lambda args (error "not implemented")))
(define sys/fork (lambda args (error "not implemented")))
(define sys/open (lambda args (error "not implemented")))
(define sys/wait (lambda args (error "not implemented")))
