(define (sys/read fd n)
  (let ((c (peek-char)))
    (if (eof-object? c)
      ""
      (string (read-char)))))

(define (sys/write fd s n)
  (display s)) ; hack

(define sys/exit exit)

(define / quotient)

(define (string-length s) (bytevector-length (string->utf8 s)))

(define (string-list s)
  (map string (string->list s)))

(define function? procedure?)

(define native-funcs
  '(
    +
    -
    *
    /
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
    filter
    enumerate
    eof?
    equal?
    error
    find
    first
    for-each
    function?
    length
    last
    list
    list?
    map
    modulo
    newline
    not
    null?
    number->string
    number?
    pair?
    peek-c
    read-c
    reverse
    second
    set-car!
    set-cdr!
    string
    string->number
    string-append
    string-length
    string-list
    string-number?
    string?
    sublist
    symbol
    symbol?
    sys/exit
    sys/read
    sys/write
    sys/execve
    third
    zip
    /
    ))

(define runtime "s")
(define (xlog v)
  (display runtime)
  (display "> ")
  (display v)
  (newline))

(define e (interaction-environment))

(define native-eval eval)
(define (make-env runtime)
  (define (xlog v)
    (display runtime)
    (display "> ")
    (display v)
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

(load-option 'synchronous-subprocess)
