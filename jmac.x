; Minimal self hosting eval.
; Assumes only lambda, quote, list?, equal?, cons, car, cdr, cond.

;((lambda (Y)
;   ((lambda (null. and. not. assoc. zip.)
;     (Y (lambda eval.) ; actually returns eval.
;        ((lambda evcon. evlambda. evapply. evlist.)
;         (lambda (e a) ; eval.
;           )
;
; (lambda (f)
;   ((lambda (x)
;      (f (lambda (v) ((x x) v))))
;    (lambda (x)
;      (f (lambda (v) ((x x) v)))))))
;
;(Y (lambda (eval.)
;     ))

((lambda (null. and. not. assoc. eval. evcon. evlambda. evapply. evlist. zip.)
   eval.)
 ; null.
 (lambda (x)
  (equal? x '()))
 ; and.
 (lambda (x y)
   (cond (x (cond (y #t) (#t #f)))
         (#t #f)))
 ; not.
 (lambda (x)
   (cond (x #f)
         (#t #t)))
 ; assoc.
 (lambda (x y)
   (cond ((equal? (caar y) x) (cadar y))
         (#t (assoc. x (cdr y)))))
 ; eval.
 (lambda (e a)
  (cond ((not. (list? e)) (assoc. e a))
        (#t (cond 
          ((equal? (car e) 'quote)   (cadr e))
          ((equal? (car e) 'equal?)  (equal? (eval. (cadr e) a)
                                             (eval. (caadr e) a)))
          ((equal? (car e) 'car)     (car (eval. (cadr e) a)))
          ((equal? (car e) 'cdr)     (cdr (eval. (cadr e) a)))
          ((equal? (car e) 'cons)    (cons (eval. (cadr e) a)
                                           (eval. (caddr e) a)))
          ((equal? (car e) 'cond)    (evcon. (cdr e) a))
          ((equal? (car e) 'lambda)  (evlambda. e a))
          (#t                        (evapply. e a))))))
 ; evcon.
 (lambda (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        (#t (evcon. (cdr c) a))))
 ; evlambda.
 (lambda (e a)
   (lambda (args)
     (eval. (caddr e) (cons (zip. (cadr e) args) a))))
 ; evapply.
 (lambda (e a)
   ((eval. (car e) a) (evlist. (cdr e) a)))
 ; evlist.
 (lambda (l a)
   (cond ((null. l) '())
         (#t (cons (eval. (car l) a)
                   (evlist. (cdr l) a)))))
 ; zip.
 (lambda (x y)
   (cond ((null. x) '())
         (#t (cons (cons (car x) (cons (car y) '())) (zip. (cdr x) (cdr y))))))

