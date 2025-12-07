(define f (lambda args (apply + args)))

(f 1 2)

(define (curry f x)
  (lambda args (apply f (cons x args))))

(define add-five (curry + 5))

(add-five 10)
