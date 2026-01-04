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

(define native-with-input-from-file with-input-from-file)

(define (with-input-from-file p f)
  (let ((out (native-with-input-from-file p f)))
    out))

