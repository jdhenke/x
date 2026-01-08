; Minimal self hosting eval.
; Assumes only quote, lambda, cond, apply special forms
; Assumes only list?, equal?, cons, car, cdr functions

((lambda (y2.)
   ((lambda (and. append. assoc. bind.)
     (y2. (lambda (eval.)
       ((lambda (evcon. evlambda. evlist.)
          (lambda (e a)
            (cond ((list? e)
                   (cond 
                     ((equal? (car e) 'quote)   (car (cdr e)))
                     ((equal? (car e) 'lambda)  (evlambda. e a))
                     ((equal? (car e) 'cond)    (evcon. (cdr e) a))
                     ((equal? (car e) 'apply)   (apply (eval. (car (cdr e)) a) (eval. (car (cdr (cdr e))) a)))
                     ((equal? (car e) 'equal?)  (equal? (eval. (car (cdr e)) a)
                                                        (eval. (car (cdr (cdr e))) a)))
                     ((equal? (car e) 'car)     (car (eval. (car (cdr e)) a)))
                     ((equal? (car e) 'cdr)     (cdr (eval. (car (cdr e)) a)))
                     ((equal? (car e) 'cons)    (cons (eval. (car (cdr e)) a)
                                                      (eval. (car (cdr (cdr e))) a)))
                     (#t                        (apply (eval. (car e) a) (evlist. (cdr e) a)))))
                  (#t (assoc. e a)))))
        ; evcon.
        (y2. (lambda (evcon.)
          (lambda (c a)
           (cond ((eval. (car (car c)) a)
                  (eval. (car (cdr (car c))) a))
                 (#t (evcon. (cdr c) a))))))
        ; evlambda.
        (lambda (e a)
          (cond ((list? (car (cdr e)))
                 (lambda args
                   (eval. (car (cdr (cdr e))) (append. (bind. (car (cdr e)) args) a))))
                (#t
                 (lambda args
                   (eval. (car (cdr (cdr e))) (cons (cons (car (cdr e)) (cons args '())) a))))))
        ; evlist.
        (y2. (lambda (evlist.)
               (lambda (l a)
                 (cond ((equal? l '()) '())
                       (#t (cons (eval. (car l) a)
                                 (evlist. (cdr l) a)))))))))))
    ; and.
    (lambda (x y)
      (cond (x (cond (y #t) (#t #f)))
            (#t #f)))
    ; append.
    (y2. (lambda (append.)
      (lambda (x y)
        (cond ((equal? x '()) y)
              (#t (cons (car x) (append. (cdr x) y)))))))
    ; assoc.
    (y2. (lambda (assoc.)
      (lambda (x y)
        (cond ((equal? (car (car y)) x) (car (cdr (car y))))
              (#t (assoc. x (cdr y)))))))
    ; bind.
    (y2. (lambda (bind.)
      (lambda (x y)
        (cond ((equal? x '()) '())
              (#t (cons (cons (car x) (cons (car y) '())) (bind. (cdr x) (cdr y))))))))))
 ; y2.
 (lambda (f)
   ((lambda (x)
      (f (lambda (v1 v2) ((x x) v1 v2))))
    (lambda (x)
      (f (lambda (v1 v2) ((x x) v1 v2)))))))

