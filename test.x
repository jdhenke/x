(define f (lambda args (apply + args)))

(f 1 2)

(define (curry f . largs)
  (lambda rargs (apply f (append largs rargs))))

(define add-five (curry + 5))

(add-five 10)

(define add-five-and-six (curry + 5 6))

(add-five-and-six 9 10)

(or #f #t)
(or #f #f #f)
(and #f #f #f)
(and #t #t #t)
