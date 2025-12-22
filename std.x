(define (reverse l)
  (let loop ((l l) (t (list)))
    (if (null? l)
      t
      (loop (cdr l) (cons (car l) t)))))

(define (append . args)
  (let arglp ((args args)
               (out (list)))
    (if (null? args)
      (reverse out)
      (arglp (cdr args)
             (let itemlp ((l (car args))
                          (out out))
               (if (null? l)
                 out
                 (itemlp (cdr l) (cons (car l) out))))))))

(define (curry f . largs)
  (lambda rargs
    (apply f (append largs rargs))))

(define (cadr x)
  (car (cdr x)))

(define (find f l)
  (let loop ((l l))
    (if (null? l)
      #f
      (if (f (car l))
        (car l)
        (loop (cdr l))))))

(define (map f l)
  (let loop ((l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (cdr l) (cons (f (car l)) out)))))
