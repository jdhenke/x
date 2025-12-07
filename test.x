(define (read-symbol)
  (symbol
    (read-matching
      (lambda (c) 
        (not
          (or
            (eof-object? c)
            (equal? c ")")
            (equal? c " ")
            (equal? c "\n")))))))
