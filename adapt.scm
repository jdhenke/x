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

(define global
  (list
    (append
      (list
        (list "runtime" "x"))
      (map (lambda (s)
             (list (string s)
                   (eval s (interaction-environment))))
           native-funcs))
    #f))

(set! global (list (list (list "global" global)) global))

(define runtime "")

