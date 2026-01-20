; eval
(define env (interaction-environment))

; eval.
(define eval. (car (read-file "eval.scm")))
(define env.
  (list
    (list '#t #t)
    (list '#f #f)
    (list 'list? list?)
    (list 'equal? equal?)
    (list 'cons cons)
    (list 'car car)
    (list 'cdr cdr)))

; reverse.
(define reverse.
  (quote
    ((lambda (y.)
       ((lambda (append.)
         (y.
           (lambda (reverse.)
             (lambda (l)
               (cond ((equal? l '()) l)
                      (#t (append. (reverse. (cdr l)) (cons (car l) '()))))))))
         (y.
           (lambda (append.)
             (lambda (x y)
               (cond ((equal? x '()) y)
                     (#t (cons (car x) (append. (cdr x) y)))))))))
     (lambda (f)
       ((lambda (x)
          (f (lambda args (apply (x x) args))))
        (lambda (x)
          (f (lambda args (apply (x x) args)))))))))

; progressively nest
(for-each
  (lambda (x)
    (pretty-print x)
    (newline)
    (pretty-print (eval x env))
    (newline)
    (newline))
  '(
    reverse.
    ((eval reverse. env) '(a b c d e f))
    (((eval eval. env) reverse. env.) '(a b c d e f))
    ((((eval eval. env) eval. env.) reverse. env.) '(a b c d e f))
    ;(((((eval eval. env) eval. env.) eval. env.) reverse. env.) '(a b c d e f)) ; takes a few minutes
    ))

