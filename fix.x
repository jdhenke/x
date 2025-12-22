(define (find f l)
  (let loop ((l l))
    (if (null? l)
      42
      (if (f (car l))
        (car l)
        (loop (cdr l))))))

(println (find (lambda (r) (equal? (car r) "b")) (list (list "a" 1) (list "b" 2))))
