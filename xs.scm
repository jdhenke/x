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

(define native-read-string read-string)
(define native-read read)

(define (read-file-no-pairs filename)
  (let* ((text (call-with-input-file filename native-read-string))
         (port (open-input-string text)))
    (let loop ((exprs '()))
      (let ((expr (native-read port)))
        (if (eof-object? expr)
          (reverse exprs)
          (loop (cons expr exprs)))))))
