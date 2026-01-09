(define jmac (car (read-file "jmac.x")))

(define env (list
              (list '#t #t)
              (list '#f #f)
              (list 'list? list?)
              (list 'equal? equal?)
              (list 'cons cons)
              (list 'car car)
              (list 'cdr cdr)))

(display "JMAC 1")
(newline)
(define jmac1 (eval jmac (interaction-environment)))
(pretty-print (jmac1 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

(display "JMAC 2")
(newline)
(define jmac2 (jmac1 jmac env))
(pretty-print (jmac2 'b '((b 52))))
(newline)
(pretty-print (jmac2 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

(display "JMAC 3")
(newline)
(define jmac3 (jmac2 jmac env))
(pretty-print (jmac3 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

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

(display "JMAC1 - FIB 10")
(newline)
(define j1fib (jmac1 fib fibenv))
(pretty-print (j1fib 10))
(newline)

(display "JMAC2 - FIB 10")
(newline)
(define j2fib (jmac2 fib fibenv))
(pretty-print (j2fib 10))
(newline)

