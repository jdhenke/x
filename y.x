; Y = λf.(λx.f(x x))(λx.f(x x))
;(define (Y f)
;    ((lambda (x) (f (lambda (v) ((x x) v))))
;     (lambda (x) (f (lambda (v) ((x x) v))))))

; anonymous
;(lambda (f)
;    ((lambda (x) (f (lambda (v) ((x x) v))))
;     (lambda (x) (f (lambda (v) ((x x) v))))))

(((lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))
  (lambda (self)
    (lambda (n)
      (pp `((((lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v)))))) (lambda (self) ,n))) ,n)))))
 '(lambda (n)
      (pp `((((lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v)))))) (lambda (self) ,n))) ,n))))

;(define fib
;  (Y (lambda (f)
;       (lambda (n)
;         (cond ((= n 0) 0)
;               ((= n 1) 1)
;               (else (+ (f (- n 1))
;                       (f (- n 2)))))))))

;(display (fib 5))
;(newline)

