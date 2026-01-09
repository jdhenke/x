(define jmac (car (read-file "jmac.x")))

(define env (list
              (list '#t #t)
              (list '#f #f)
              (list 'list? list?)
              (list 'equal? equal?)
              (list 'cons cons)
              (list 'car car)
              (list 'cdr cdr)))

(display "JMC 1")
(newline)
(define jmac1 (eval jmac (interaction-environment)))

(display "JMC 2")
(newline)
(define jmac2 (jmac1 jmac env))

(define fib
  (quote
    ((lambda (y)
       (y
         (lambda (fib)
           (lambda (n)
             (cond ((< n three) one)
                   (#t (+ (fib (- n one)) (fib (- n two)))))))))
     (lambda (f)
       ((lambda (x)
          (f (lambda args (apply (x x) args))))
        (lambda (x)
          (f (lambda args (apply (x x) args)))))))))

(pretty-print fib)
(newline)

(define fibenv (append (list 
                         (list 'one 1)
                         (list 'two 2)
                         (list 'three 3)
                         (list '< <)
                         (list '- -)
                         (list '+ +))
                       env))

(display "JMC1 - FIB 10")
(newline)
(define j1fib (jmac1 fib fibenv))
(pretty-print (j1fib 10))
(newline)

(display "JMC2 - FIB 10")
(newline)
(define j2fib (jmac2 fib fibenv))
(pretty-print (j2fib 10))
(newline)

