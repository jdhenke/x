;(+ 1 2)
;(+ (+ 1 2) 3 4 5)
;(define (add-five x) (+ x 5))
;(add-five 5)
;(define (twice f)
;  (define (out x)
;    (f (f x)))
;  out)
;
;((twice add-five) 4)
;
;(define (id x) x)
;
;((id add-five) 7)
;
;((twice (lambda (x) (+ x 6))) 10)
;
;(define (fib x)
;  (cond ((< x 3) 1)
;        (1 (+ (fib (- x 1)) (fib (- x 2))))))
;
;(fib 1)
;(fib 2)
;(fib 3)
;(fib 4)
;(fib 5)
;(fib 6)

(let ((a 1))
  (+ a 2))

(let fib ((n 7))
  (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(let ((a 1)
      (b (+ a 1)))
  (+ a b))

(apply + (list 1 2 3))
