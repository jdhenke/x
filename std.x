;;; OUTPUT

(define (print x)
  (cond
    ((string? x)    (sys/write 1 x (string-length x)))
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

;;; BOOLEANS

(define (not x)
  (if x #f #t))

;;; INTEGERS

(define = equal?)

(define (> a b) (< b a))

(define (>= a b)
  (< b (+ a 1)))

(define (<= a b) (>= b a))

;;; LISTS

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (fourth l) (cadddr l))
(define (last l) (first (reverse l)))

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

(define (enumerate f l)
  (let loop ((i 0)
             (l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (+ i 1) (cdr l) (cons (f i (car l)) out)))))

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

(define assq
  (if (function? assq)
    assq
    (lambda (k l) (find (lambda (r) (eq? (car r) k)) l))))

(define (list-ref l i)
  (if (= i 0)
    (car l)
    (list-ref (cdr l) (- i 1))))

(define (sort lst less-than?)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (let ((smaller (filter (lambda (x) (less-than? x pivot)) rest))
              (greater (filter (lambda (x) (not (less-than? x pivot))) rest)))
          (append (sort smaller less-than?)
                  (list pivot)
                  (sort greater less-than?))))))

;;; STRINGS

(define (number->string x)
  (if (< x 0)
    (string-append "-" (number->string (- 0 x)))
    (if (< x 10)
      (cond ((= x 0) "0")
            ((= x 1) "1")
            ((= x 2) "2")
            ((= x 3) "3")
            ((= x 4) "4")
            ((= x 5) "5")
            ((= x 6) "6")
            ((= x 7) "7")
            ((= x 8) "8")
            ((= x 9) "9"))
      (string-append (number->string (/ x 10))
                     (number->string (modulo x 10))))))
(define (digit-string->int ds)
  (cond ((equal? ds "0") 0)
        ((equal? ds "1") 1)
        ((equal? ds "2") 2)
        ((equal? ds "3") 3)
        ((equal? ds "4") 4)
        ((equal? ds "5") 5)
        ((equal? ds "6") 6)
        ((equal? ds "7") 7)
        ((equal? ds "8") 8)
        ((equal? ds "9") 9)
        (#t #f)))

(define (string->number s)
  (let loop ((cs (string-list s)) (out 0))
    (if (null? cs)
      out
      (loop (cdr cs) (+ (* out 10) (digit-string->int (car cs)))))))

(define (string-number? s)
  (and
    (> (string-length s) 0)
    (let loop ((l (string-list s)))
      (if (null? l)
        #t
        (if (digit-string->int (car l))
          (loop (cdr l))
          #f)))))

;;; FUNCTIONS

(define curry (lambda args
  (define f (car args))
  (define largs (cdr args))
  (lambda rargs
    (apply f (append largs rargs)))))


;;; SYSCALL

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

(define (read-file p)
  (with-input-from-file p
    (lambda ()
      (let loop ((out (list)))
        (let ((sexpr (read)))
          (if (eof? sexpr)
            (reverse out)
            (loop (cons sexpr out))))))))

(define (read-std)
  (read-file "std.x"))

;;; RAND

(define make-random-state
  (if (function? make-random-state)
    make-random-state
    (lambda args #f)))

;;; READ

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

(define (read-quote)
  (read-c)
  (list 'quote (read)))

(define (read)
  (read-whitespace)
  (let ((c (peek-c)))
    (cond
      ((eof? c) c)
      ((equal? c "(")     (read-list))
      ((equal? c "#")     (read-boolean))
      ((string-number? c) (read-number))
      ((equal? c "\"")    (read-string))
      ((equal? c "'")     (read-quote))
      (#t                 (read-symbol)))))

;;; PARSE

(define (split-args args)
  (let loop ((args args)
             (named '()))
    (if (null? args)
      (list (reverse named) #f)
      (if (equal? (car args) (symbol "."))
        (list (reverse named) (cadr args))
        (loop (cdr args) (cons (car args) named))))))

(define (func-named sexpr)
  (car (split-args (cdadr sexpr))))

(define (func-rest sexpr) 
  (cadr (split-args (cdadr sexpr))))

(define (func-body sexpr) (cddr sexpr))

(define (lambda-named sexpr)
  (if (list? (second sexpr))
    (car (split-args (second sexpr)))
    (list)))

(define (lambda-rest sexpr)
  (if (list? (second sexpr))
    (cadr (split-args (second sexpr)))
    (second sexpr)))

(define (lambda-body sexpr) (cddr sexpr))
