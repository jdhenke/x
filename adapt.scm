(define (peek-c)
  (let ((c (peek-char)))
    (if (eof-object? c)
      c
      (list->string (list c)))))

(define (read-c)
  (let ((c (read-char)))
    (if (eof-object? c)
      c
      (list->string (list c)))))

(define eof? eof-object?)

(define (string-number? s)
  (not (false? (string->number s))))

(define (curry f . args)
  (lambda foo
    (apply f (append args foo))))

(define (enumerate f l)
  (let loop ((i 0)
             (l l)
             (out '()))
    (if (null? l)
      (reverse out)
      (loop (+ i 1) (cdr l) (cons (f i (car l)) out)))))

(define (string-list s)
  (map (lambda (c) (list->string (list c))) (string->list s)))

(define print display)
(define println (lambda (x)
                  (print x)
                  (newline)))

(define (sys/read fd n)
  (let ((c (read-char)))
    (if (eof? c)
      c
      (list->string (list c)))))

(define sys/exit exit)

(define native-funcs
  '(
    +
    -
    <
    >
    >=
    <=
    =
    append
    apply
    assoc
    boolean?
    caadr
    caar
    cadar
    cadddr
    caddr
    cadr
    car
    cdr
    cdadr
    cddr
    cdddr
    cons
    curry
    display
    filter
    enumerate
    eof?
    equal?
    error
    find
    first
    for-each
    length
    last
    list
    list?
    map
    newline
    not
    null?
    number->string
    number?
    peek-c
    pretty-print
    print
    println
    read-c
    reverse
    second
    set-car!
    set-cdr!
    string
    string->number
    string-append
    string-length
    string-number?
    string?
    sublist
    symbol
    symbol?
    sys/exit
    sys/read
    third
    zip
    modulo
    ))

(define runtime "s")
(define (xlog v)
  (display runtime)
  (display "> ")
  (pretty-print v)
  (newline))

(define e (interaction-environment))

(define native-eval eval)
(define (make-env runtime)
  (define (xlog v)
    (display runtime)
    (display "> ")
    (pretty-print v)
    (newline))
  (list
      (append
        (list
          (list "runtime"   runtime)
          (list "xlog"      xlog)
          (list "make-env"  make-env))
        (map (lambda (s)
               (list (string s)
                     (native-eval s e)))
             native-funcs))
      #f))


