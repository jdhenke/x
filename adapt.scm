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
    =
    append
    apply
    car
    cadr
    caddr
    cadddr
    cadar
    cdr
    cons
    curry
    eof?
    equal?
    list
    not
    peek-c
    read-c
    reverse
    string-append
    string-number?
    string->number
    symbol
    list?
    number?
    symbol?
    boolean?
    string?
    pretty-print
    newline
    ))

(define global
  (list
    (append
      (list
        (list "apply" (lambda (args) (apply (car args) (cdr args)))))
      (map (lambda (s)
             (list (string s)
                   (curry apply (eval s (interaction-environment)))))
           native-funcs))
    #f))

