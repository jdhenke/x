;;; syscalls

;;; core interpreter

(define (read)
  (read-whitespace)
  (let ((c (peek-char)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\#) (read-chr))
      ((char=? c #\') (read-literal))
      ((char=? c #\() (read-list))
      ((char=? c #\") (read-str))
      ((char-numeric? c) (read-number))
      (else (read-symbol)))))

(define (log x s)
  (display s)
  (display " ")
  (display x)
  (newline)
  x)

(define (read-chr)
  (let* ((a (read-char))
         (b (read-char))
         (s (list a b)))
    (if (char=? (cadr s) #\\)
      (set! s (append s (list (read-char)))))
    (list 'char (list->string (append s (read-char-matching (lambda (c) 
          (not
            (or
              (eof-object? c)
              (char=? c #\))
              (char-whitespace? c)
              (char=? c #\newline))))))))))

(define (read-literal)
  (read-char)
  (list 'literal (read)))

(define (read-list)
  (read-char)
  (read-whitespace)
  (let loop ((vals '()))
    (if (char=? (peek-char) #\))
      (begin
        (read-char)
        (list 'list vals))
      (let ((val (read)))
        (if (eof-object? val)
          (error "unexpected EOF reading list")
          (loop (append vals (list val))))))))

(define (read-whitespace)
  (read-char-matching (lambda (c) (or (char-whitespace? c) (char=? c #\newline))))
  (let ((c (peek-char)))
    (if (eof-object? c)
      c
      (if (char=? (peek-char) #\;)
        (begin
          (read-char-matching (lambda (c) (not (char=? c #\newline))))
          (read-whitespace))
        #f))))

(define (read-number)
  (list 'number (string->number (list->string (read-char-matching char-numeric?)))))

(define (read-char-matching f)
  (let ((c (peek-char)))
    (cond ((or (eof-object? c) (not (f c))) (list))
          (else (begin
                  (read-char) 
                  (cons c (read-char-matching f)))))))

(define (read-symbol)
  (list
    'symbol
    (list->string
      (read-char-matching
        (lambda (c) 
          (not
            (or
              (eof-object? c)
              (char=? c #\))
              (char-whitespace? c)
              (char=? c #\newline))))))))

(define (read-str)
  (read-char)
  (let ((s (read-char-matching (lambda (c) (not (char=? c #\"))))))
    (if (char=? (peek-char) #\")
      (begin
        (read-char)
        (list 'string (list->string s)))
      (error "invalid end of string" (peek-char)))))

;;; bytes --(read)-> sexpr --(eval)--> value


;;; value -> number, string, list, function
;;; ("number" 4)
;;; ("string" "hello")
;;; ("list" (...value...))
;;; ("function" env args (...sexpr...))

(define (eval sexpr env) ;; --> Value
  (let ((type (car sexpr)))
    (cond ((or
             (equal? type 'string)
             (equal? type 'literal)
             (equal? type 'char)
             (equal? type 'number)) sexpr)
          ((equal? type 'symbol)
           (lookup env (cadr sexpr)))
          ((equal? type 'list)
           (eval-verb sexpr env))
          (else #f))))

(define (lookup env name)
  (let ((defs (car env))
        (parent (cadr env)))
    (let ((d (find (lambda (d) (equal? (car d) name)) defs)))
      (if d
        (cadr d)
        (if parent (lookup parent name) (error (string-append "undefined: " name)))))))

(define (eval-verb sexpr env)
  (let ((l (cadr sexpr)))
    (cond ((null? l) (list "list" '()))
          ((equal? (cadar l) "define") (define-func sexpr env))
          (else (apply-func (eval (car l) env) (map (lambda (s) (eval s env)) (cdr l)))))))

(define (define-func sexpr env)
  (let* ((clause (cadadr sexpr))
         (names (map cadr (cadr clause)))
         (funcname (car names))
         (argnames (cdr names))
         (body (cddadr sexpr)))
    (define f (list
      "function"
      (lambda (args)
        (let ((env (list (cons (list funcname f) (zip argnames args)) env)))
          (let loop ((body body) (last #f))
            (if (null? body)
              last
              (loop (cdr body) (eval (car body) env))))))))
    (set-car! env (cons (list funcname f) (car env)))
    f))

(define (apply-func f args)
  ((cadr f) args))

(define global
  (list
    (list
      (list "+" (list "function" (lambda (args) (begin (list "number" (apply + (map cadr args))))))))
    #f))

(let loop ()
  (let ((sexpr (read)))
    (if (eof-object? sexpr)
      #t
      (begin 
        (pretty-print (eval sexpr global))
        (newline)
        (loop)))))

