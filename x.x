;;; native functions

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
  (read-whitespace)
  (let loop ((vals (list)))
    (if (equal? (peek-c) ")")
      (let ()
        (read-c)
        (reverse vals))
      (let ((val (read)))
        (if (eof? val)
          (error "unexpected EOF reading list")
          (loop (cons val vals)))))))

(define (read-boolean)
  (read-c)
  (equal? (read-c) "t"))

(define (read-number)
  (string->number (read-matching string-number?)))

(define (read-string)
  (read-c)
  (let ((s (read-matching (lambda (c) (not (equal? c "\""))))))
    (if (equal? (peek-c) "\"")
      (let ()
        (read-c)
        s)
      (error "unexpected EOF reading string" (peek-char)))))

(define (read-symbol)
  (symbol
    (read-matching
      (lambda (c) 
        (not
          (or
            (eof-object? c)
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
      (#t                (read-symbol)))))

;;; EVAL

(define (eval sexpr env)
  (cond ((list? sexpr)    (eval-verb sexpr env))
        ((boolean? sexpr) sexpr)
        ((number? sexpr)  sexpr)
        ((string? sexpr)  sexpr)
        ((symbol? sexpr)  (lookup env sexpr))
        (#t (error "unknown sexpr type" sexpr))))

(define (eval-verb sexpr env)
  (cond ((null? sexpr) sexpr)
        ((equal? (car sexpr) (symbol "define")) (eval-define sexpr env))
        ((equal? (car sexpr) (symbol "lambda")) (define-lambda sexpr env))
        ((equal? (car sexpr) (symbol "if"))     (eval-if sexpr env))
        ((equal? (car sexpr) (symbol "cond"))   (eval-cond sexpr env))
        ((equal? (car sexpr) (symbol "let"))    (eval-let sexpr env))
        ((equal? (car sexpr) (symbol "let*"))   (eval-let sexpr env))
        ((equal? (car sexpr) (symbol "apply"))  (apply-func sexpr env))
        (#t                                     (call-func sexpr env))))

(define (eval-define sexpr env)
  (if (list? (second sexpr))
    (define-func sexpr env)
    (define-var sexpr env)))

(define (define-func sexpr env)
  (let ((funcname (string (caadr sexpr)))
        (argnames (map string (cdadr sexpr)))
        (body (cddr sexpr)))
    (define f (lambda (args)
      (let ((env (list (cons (list funcname f) (zip argnames args)) env)))
        (let loop ((body body) (last 0))
          (if (null? body)
            last
            (loop (cdr body) (eval (car body) env)))))))
    (set-car! env (cons (list funcname f) (car env)))
    f))

(define (define-var sexpr env)
  (let ((name (string (second sexpr)))
        (val (eval (third sexpr) env)))
    (set-car! env (cons (list name val) env))
    val))

(define (define-lambda sexpr env)
  (let* ((argnames (map string (cadr sexpr)))
         (body (cddr sexpr)))
    (lambda (args)
      (let ((env (list (zip argnames args) env)))
        (let loop ((body body) (last 0))
          (if (null? body)
            last
            (loop (cdr body) (eval (car body) env))))))))

(define (eval-if sexpr env)
  (let* ((p (cadr sexpr))
         (t (caddr sexpr))
         (f (cadddr sexpr))
         (pv (eval p env)))
    (eval (if pv t f) env)))

(define (eval-cond sexpr env)
  (let loop ((conds (cdr sexpr)))
    (if (null? conds)
      #f
      (if (eval (caar conds) env)
        (eval (cadar conds) env)
        (loop (cdr conds))))))

(define (eval-let sexpr env)
  (let* ((let-name (if (list? (second sexpr)) #f (string (second sexpr))))
         (arg-clause (find list? sexpr))
         (argnames (map (lambda (p) (string (car p))) arg-clause))
         (body (if let-name (cdddr sexpr) (cddr sexpr))))
    (let ((env (list (list) env)))
      (define (f args)
        (let ((env (list (zip argnames args) env)))
          (let body-loop ((body body) (last #f))
            (if (null? body)
              last
              (body-loop (cdr body) (eval (car body) env))))))
      (if let-name (set-car! env (list (list let-name f))) #f)
      (let arg-loop ((exprs arg-clause)
                     (vals (list)))
        (if (null? exprs)
          (f (reverse vals))
          (let* ((arg-env (list (zip argnames (reverse vals)) env))
                 (val (eval (cadar exprs) arg-env)))
            (arg-loop (cdr exprs) (cons val vals))))))))

(define (apply-func sexpr env)
  (let ((f (eval (second sexpr) env))
        (args (eval (third sexpr) env)))
    (f args)))

(define (call-func sexpr env)
  (let ((f (eval (car sexpr) env))
        (args (map (lambda (s) (eval s env)) (cdr sexpr))))
    (f args)))

(define (lookup env sexpr)
  (let ((name (string sexpr))
        (defs (car env))
        (parent (cadr env)))
    (let ((d (find (lambda (d) (equal? (car d) name)) defs)))
      (if d
        (cadr d)
        (if parent (lookup parent name) (error (string-append "undefined: " name)))))))

(define global
  (list
    (list
      (list "+" (curry apply +))
      (list "<" (curry apply <))
      (list "-" (curry apply -))
      (list "list" (curry apply list)))
    #f))

;;; REPL

(let repl ()
  (let ((sexpr (read)))
    (if (eof? sexpr)
      0
      (let ()
        (pretty-print (eval sexpr global))
        (newline)
        (repl)))))

