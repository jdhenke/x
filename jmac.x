; Minimal self hosting eval.
; Assumes only quote, lambda, cond, apply special forms.
; Assumes only #t, #f, list?, equal?, cons, car, cdr values.

((lambda (y.)
   ((lambda (append. assoc. bind.)
     (y. (lambda (eval.)
       ((lambda (evcond. evlambda. evlist.)
          (lambda (e a)
            (cond ((list? e)
                   (cond 
                     ((equal? (car e) 'quote)   (car (cdr e)))
                     ((equal? (car e) 'lambda)  (evlambda. e a))
                     ((equal? (car e) 'cond)    (evcond. (cdr e) a))
                     ((equal? (car e) 'apply)   (apply (eval. (car (cdr e)) a) (eval. (car (cdr (cdr e))) a)))
                     (#t                        (apply (eval. (car e) a) (evlist. (cdr e) a)))))
                  (#t (assoc. e a)))))

        (y. (lambda (evcond.)
          (lambda (c a)
           (cond ((eval. (car (car c)) a)
                  (eval. (car (cdr (car c))) a))
                 (#t (evcond. (cdr c) a))))))
        ; evlambda.
        (lambda (e a)
          (lambda args
            (eval. (car (cdr (cdr e)))
                   (cond ((list? (car (cdr e))) (append. (bind. (car (cdr e)) args) a))
                         (#t                    (cons (cons (car (cdr e)) (cons args '())) a))))))

        (y. (lambda (evlist.)
          (lambda (l a)
            (cond ((equal? l '()) '())
                   (#t (cons (eval. (car l) a)
                             (evlist. (cdr l) a)))))))))))
    (y. (lambda (append.)
      (lambda (x y)
        (cond ((equal? x '()) y)
              (#t (cons (car x) (append. (cdr x) y)))))))

    (y. (lambda (assoc.)
      (lambda (x y)
        (cond ((equal? (car (car y)) x) (car (cdr (car y))))
              (#t (assoc. x (cdr y)))))))

    (y. (lambda (bind.)
      (lambda (x y)
        (cond ((equal? x '()) '())
              (#t (cons (cons (car x) (cons (car y) '())) (bind. (cdr x) (cdr y))))))))))

 (lambda (f)
   ((lambda (x)
      (f (lambda args (apply (x x) args))))
    (lambda (x)
      (f (lambda args (apply (x x) args)))))))

