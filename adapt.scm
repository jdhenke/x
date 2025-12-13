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


(define native-funcs
  '(
    +
    -
    <
    >
    =
    append
    apply
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
    eof?
    equal?
    error
    find
    first
    length
    last
    list
    list?
    map
    newline
    not
    null?
    number?
    peek-c
    pretty-print
    read-c
    reverse
    second
    set-car!
    set-cdr!
    string
    string->number
    string-append
    string-number?
    string?
    sublist
    symbol
    symbol?
    third
    zip
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


