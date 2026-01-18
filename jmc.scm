(display "fib-def:")
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
(newline)


(display "((eval fib-def) 10))")
(newline)
(define one 1)
(define two 2)
(define three 3)
(define e (interaction-environment))
(pretty-print ((eval fib e) 10))
(newline)
(newline)


(display "(((eval eval-def) fib-def) 10)")
(newline)
(define mvp (car (read-file "mvp.scm")))
(define env (list
              (list '#t #t)
              (list '#f #f)
              (list 'list? list?)
              (list 'equal? equal?)
              (list 'cons cons)
              (list 'car car)
              (list 'cdr cdr)))
(define fibenv (append (list
                         (list 'one 1)
                         (list 'two 2)
                         (list 'three 3)
                         (list '< <)
                         (list '- -)
                         (list '+ +))
                       env))
(pretty-print (((eval mvp e) fib fibenv) 10))
(newline)
(newline)

(display "((((eval eval-def) eval-def) fib-def) 10)")
(newline)
(pretty-print ((((eval mvp e) mvp env) fib fibenv) 10))
(newline)

;(display "(((((eval eval-def) eval-def) eval-def) fib-def) 10)")
;(newline)
;(pretty-print (((((eval mvp e) mvp env) mvp env) fib fibenv) 10))
;(newline)

