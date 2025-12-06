(+ 1 2)
(+ (+ 1 2) 3 4 5)
(define (add-five x) (+ x 5))
(add-five 5)
(define (twice f)
  (define (out x)
    (f (f x)))
  out)

((twice add-five) 4)

(define (id x) x)

((id add-five) 7)

((twice (lambda (x) (+ x 6))) 10)
