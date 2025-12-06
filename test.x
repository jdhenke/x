(define (add-five x) (+ x 5))
(+ 1 2)
(+ (+ 1 2) 3)
(+ (+ 1 2) 3 4 5)
(add-five 5)

(define (add-five-twice x)
  (add-five (add-five x)))

(add-five-twice 10)

(define (id x) x)

((id add-five) 7)

(define (self) self)

(self)

(define (twice f)
  (define (out x)
    (f (f x)))
  out)

((twice add-five) 4)

