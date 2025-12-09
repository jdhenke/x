(define (curry f . largs)
  (lambda rargs (apply f (append largs rargs))))

(define (memo f)
  (let ((m (list)))
    (lambda args
      (let ((r (find (lambda (r) (equal? (car r) args)) m)))
        (if r
          (cadr r)
          (let ((r (apply f args)))
            (set! m (cons (list args r) m))
            r))))))

(define (fib n)
  (if (< n 3)
    1
    (apply + (map fib (map (curry - n) (list 1 2))))))

(set! fib (memo fib))

(let loop ((i 1))
  (if (< i 11)
    (let () 
      (xlog (fib i))
      (loop (+ i 1)))))
