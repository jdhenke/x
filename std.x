(define (not x)
  (if x #f #t))

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

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

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

(define eof (lambda () #f)) ; sepxr will never be a function

(define _peek-c #f)

(define (peek-c)
  (if (equal? _peek-c #f)
    (set! _peek-c
      (let ((c (sys/read 0 1)))
        (if (equal? c "")
          eof
          c))))
  _peek-c)

(define (read-c)
  (let ((out (peek-c)))
    (set! _peek-c #f)
    out))
  
(define (eof? c) (equal? c eof))

(define (error . args)
  (println "ERROR")
  (map println args)
  (sys/exit 1))

(define (enumerate f l)
  (let loop ((i 0)
             (l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (+ i 1) (cdr l) (cons (f i (car l)) out)))))

(define (>= a b)
  (< b (+ a 1)))

(define (first l) (car l))
(define (second l) (cadr l))

(define (filter f l)
  (let loop ((l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (cdr l)
            (if (f (car l))
              (cons (car l) out)
              out)))))

(define for-each map)

(define (> a b) (< b a))

(define = equal?)

(define (assoc k l)
  (find (lambda (r) (equal? (car r) k)) l))

(define display print)
