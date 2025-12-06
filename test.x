(define (add-five x) (+ x 5))
(+ 1 2)
(+ (+ 1 2) 3)
(+ (+ 1 2) 3 4 5)
(add-five 5)

(define (add-five-twice x)
  (add-five (add-five x)))

(add-five-twice 10)
