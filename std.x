(define (not x)
  (if x #f #t))

(define (reverse l)
  (let loop ((l l) (t (list)))
    (if (null? l)
      t
      (loop (cdr l) (cons (car l) t)))))

(define append (lambda args
  (let arglp ((args args)
               (out (list)))
    (if (null? args)
      (reverse out)
      (arglp (cdr args)
             (let itemlp ((l (car args))
                          (out out))
               (if (null? l)
                 out
                 (itemlp (cdr l) (cons (car l) out)))))))))

(define curry (lambda args
  (define f (car args))
  (define largs (cdr args))
  (lambda rargs
    (apply f (append largs rargs)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
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
  (if (eof? _peek-c) (error "reading after EOF"))
  (let ((out (peek-c)))
    (set! _peek-c #f)
    out))
  
(define (eof? c) (equal? c eof))

(define (print x)
  (cond
    ((string? x) (sys/write 1 x (string-length x)))
    ((symbol? x)    (print (string x)))
    ((boolean? x)   (print (if x "#t" "#f")))
    ((number? x)    (print (number->string x)))
    ((function? x)  (print "λ"))
    ((list? x)
     (let ()
       (print "(")
       (let loop ((l x))
         (if (null? l)
           (print ")")
           (let ()
             (print (car l))
             (if (not (null? (cdr l)))
               (print " "))
             (loop (cdr l)))))))))

(define (println x)
  (print x)
  (print "\n"))

(define (newline)
  (print "\n"))

(define error (lambda args
  (println "ERROR")
  (map println args)
  (sys/exit 1)))

(define (enumerate f l)
  (let loop ((i 0)
             (l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (+ i 1) (cdr l) (cons (f i (car l)) out)))))

(define (>= a b)
  (< b (+ a 1)))

(define (<= a b) (>= b a))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (last l) (first (reverse l)))

(define zip (lambda ls
  (let loop ((ls ls)
             (out (list)))
    (if (find null? ls)
      (reverse out)
      (loop (map cdr ls) (cons (map car ls) out))))))

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

(define (sublist l i j)
  (let loop ((l l)
        (i i)
        (j j)
        (out (list)))
    (cond ((> i 0) (loop (cdr l) (- i 1) (- j 1) out))
          ((> j 0) (loop (cdr l) 0 (- j 1) (cons (car l) out)))
          (#t (reverse out)))))

(define n-to-s
  (list (list 0 "0")
        (list 1 "1")
        (list 2 "2")
        (list 3 "3")
        (list 4 "4")
        (list 5 "5")
        (list 6 "6")
        (list 7 "7")
        (list 8 "8")
        (list 9 "9")))

(define s-to-n
  (list (list "0" 0)
        (list "1" 1)
        (list "2" 2)
        (list "3" 3)
        (list "4" 4)
        (list "5" 5)
        (list "6" 6)
        (list "7" 7)
        (list "8" 8)
        (list "9" 9)))

(define (number->string x)
  (define out (list))
  (if (< x 0)
    (let () (set! x (- 0 x)) (set! out (cons "-" out))))
  (let loop ((x x) (out out))
    (if (> x 9)
      (let ((r (modulo x 10)))
        (loop (/ x 10) (cons (cadr (assoc r n-to-s)) out)))
      (apply string-append (cons (cadr (assoc x n-to-s)) out)))))

(define (string->number s)
  (let loop ((cs (string-list s)) (out 0))
    (if (null? cs)
      out
      (loop (cdr cs) (+ (* out 10) (cadr (assoc (car cs) s-to-n)))))))

(define (string-number? s)
  (and
    (> (string-length s) 0)
    (let loop ((l (string-list s)))
      (if (null? l)
        #t
        (if (assoc (car l) s-to-n)
          (loop (cdr l))
          #f)))))

(define assq
  (if (function? assq)
    assq
    (lambda (k l) (find (lambda (r) (eq? (car r) k)) l))))

;;; hack around syscalls

(define with-input-from-file
  (if (function? with-input-from-file)
    (let ((orig with-input-from-file))
      (lambda (p f)
        (define old _peek-c)
        (set! _peek-c #f)
        (define out (orig p f)) ; call
        (set! _peek-c old)
        out))
    (lambda (p f)
      (let ((fd (sys/open p 0)))
        (let ((original (sys/dup 0)))
          (sys/dup2 fd 0)
          (sys/close fd)
          (define old _peek-c)
          (set! _peek-c #f)
          (define out (f)) ; call
          (set! _peek-c old)
          (sys/dup2 original 0)
          (sys/close original)
          out)))))

(define with-output-to-file
  (if (function? with-output-to-file)
    with-output-to-file
    (lambda (p f)
      (let ((fd (sys/open p 1537 438)))
        (let ((original (sys/dup 1)))
          (sys/dup2 fd 1)
          (sys/close fd)
          (let ((out (f)))
            (sys/dup2 original 1)
            (sys/close original)
            out))))))

(define run-synchronous-subprocess
  (if (function? run-synchronous-subprocess)
    run-synchronous-subprocess
    (lambda (exe args)
      (let ((pid (sys/fork)))
        (if (= 0 pid)
          (sys/execve exe (cons exe args) (list))
          (sys/wait pid))))))

(define (read-std)
  (with-input-from-file "std.x"
    (lambda ()
      (let loop ((out (list)))
        (let ((sexpr (read)))
          (if (eof? sexpr)
            (reverse out)
            (loop (cons sexpr out))))))))

;;; READ

(define (read-matching f)
  (apply string-append
    (let loop ((matched (list)))
      (let ((c (peek-c)))
        (if (or (eof? c) (not (f c)))
          (reverse matched)
          (loop (cons (read-c) matched)))))))

(define (read-whitespace)
  (read-matching
    (lambda (c)
      (or
        (equal? c " ")
        (equal? c "\n"))))
  (let ((c (peek-c)))
    (if (eof? c)
      c
      (if (equal? (peek-c) ";")
        (let ()
          (read-matching (lambda (c) (not (equal? c "\n"))))
          (read-whitespace))
        0))))

(define (read-list)
  (read-c)
  (let loop ((vals (list)))
    (read-whitespace)
    (if (equal? (peek-c) ")")
      (let ()
        (read-c)
        (reverse vals))
      (let ((val (read)))
        (if (eof? val)
          (error "unexpected EOF reading list" (reverse vals))
          (loop (cons val vals)))))))

(define (read-boolean)
  (read-c)
  (equal? (read-c) "t"))

(define (read-number)
  (string->number (read-matching string-number?)))

(define (read-string)
  (read-c)
  (let loop ((cs (list)))
    (let ((c (read-c)))
      (if (eof? c) (error "unexpected EOF reading string" (apply string-append (reverse cs))))
      (if (equal? c "\"")
        (let () (apply string-append (reverse cs)))
        (let ()
          (if (equal? c "\\")
              (let ()
                (set! c (let ((ec (read-c)))
                  (cond ((equal? ec "\"") "\"")
                        ((equal? ec "n") "\n")
                        ((equal? ec "\\") "\\")
                        (#t (error "unrecognized escape" ec))))))
              #f)
          (loop (cons c cs)))))))

(define (read-symbol)
  (symbol
    (read-matching
      (lambda (c)
        (not
          (or
            (eof? c)
            (equal? c ")")
            (equal? c " ")
            (equal? c "\n")))))))

(define (read)
  (read-whitespace)
  (let ((c (peek-c)))
    (cond
      ((eof? c) c)
      ((equal? c "(")     (read-list))
      ((equal? c "#")     (read-boolean))
      ((string-number? c) (read-number))
      ((equal? c "\"")    (read-string))
      (#t                 (read-symbol)))))
