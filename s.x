(if 1 2 3)
(if 0 2 3)

(define (fib x)
  (if (< x 3) 1 (+ (fib (- x 1)) (fib (- x 2)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
