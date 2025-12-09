(define (curry f . largs)
  (lambda rargs (apply f (append largs rargs))))

(define (fib n)
  (if (< n 3)
    1
    (apply + (map fib (map (curry - n) (list 1 2))))))

(let loop ((i 1))
  (if (< i 10)
    (let () 
      (pretty-print (fib i))
      (newline)
      (loop (+ i 1)))))
