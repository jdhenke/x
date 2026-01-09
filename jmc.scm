(define jmc (car (read-file "jmc.x")))

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
(define jmc1 (eval jmc (interaction-environment)))

(display "JMC 2")
(newline)
(define jmc2 (jmc1 jmc env))

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
(define j1fib (jmc1 fib fibenv))
(pretty-print (j1fib 10))
(newline)

(display "JMC2 - FIB 10")
(newline)
(define j2fib (jmc2 fib fibenv))
(pretty-print (j2fib 10))
(newline)

