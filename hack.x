(define (to-cps f) (lambda args ((car args) (apply f (cdr args)))))
(define sys/close/cps (to-cps sys/close))

