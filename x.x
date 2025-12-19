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
        ((equal? (car sexpr) (symbol "define"))  (eval-define sexpr env))
        ((equal? (car sexpr) (symbol "set!"))    (eval-set! sexpr env))
        ((equal? (car sexpr) (symbol "lambda"))  (define-lambda sexpr env))
        ((equal? (car sexpr) (symbol "let"))     (eval-let sexpr env))
        ((equal? (car sexpr) (symbol "let*"))    (eval-let sexpr env))
        ((equal? (car sexpr) (symbol "if"))      (eval-if sexpr env))
        ((equal? (car sexpr) (symbol "cond"))    (eval-cond sexpr env))
        ((equal? (car sexpr) (symbol "or"))      (eval-or sexpr env))
        ((equal? (car sexpr) (symbol "and"))     (eval-and sexpr env))
        (#t                                      (call-func sexpr env))))

(define (eval-define sexpr env)
  (if (list? (second sexpr))
    (define-func sexpr env)
    (define-var sexpr env)))

(define (bind-func-args argnames args)
  (if (and (> (length argnames) 1) (equal? (second (reverse argnames)) "."))
    (cons (list (last argnames) (sublist args (- (length argnames) 2) (length args)))
          (zip (sublist argnames 0 (- (length argnames) 2)) args))
    (zip argnames args)))

(define (define-func sexpr env)
  (let ((funcname (string (caadr sexpr)))
        (argnames (map string (cdadr sexpr)))
        (body (cddr sexpr)))
    (define f (lambda args
      (let ((env (list (bind-func-args argnames args) env)))
        (let loop ((body body) (last #f))
          (if (null? body)
            last
            (loop (cdr body) (eval (car body) env)))))))
    (set-car! env (cons (list funcname f) (car env)))
    f))

(define (define-var sexpr env)
  (let ((name (string (second sexpr)))
        (val (eval (third sexpr) env)))
    (set-car! env (cons (list name val) (car env)))
    val))

(define (define-lambda sexpr env)
  (lambda args 
    (let* ((binds (if (list? (second sexpr))
                    (zip (map string (cadr sexpr)) args)
                    (list (list (string (second sexpr)) args))))
           (lenv (list binds env)))
      (let loop ((body (cddr sexpr)) (last 0))
        (if (null? body)
          last
          (loop (cdr body) (eval (car body) lenv)))))))

(define (eval-if sexpr env)
  (let* ((p (cadr sexpr))
         (t (caddr sexpr))
         (f (if (> (length sexpr) 3) (cadddr sexpr) #f))
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
      (define (f . args)
        (let ((env (list (zip argnames args) env)))
          (let body-loop ((body body) (last #f))
            (if (null? body)
              last
              (body-loop (cdr body) (eval (car body) env))))))
      (if let-name (set-car! env (list (list let-name f))) #f)
      (let arg-loop ((exprs arg-clause)
                     (vals (list)))
        (if (null? exprs)
          (apply f (reverse vals))
          (let* ((arg-env (list (zip argnames (reverse vals)) env))
                 (val (eval (cadar exprs) arg-env)))
            (arg-loop (cdr exprs) (cons val vals))))))))

(define (eval-or sexpr env)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) #f)
          ((eval (car clauses) env) #t)
          (#t (loop (cdr clauses))))))

(define (eval-and sexpr env)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) #t)
          ((not (eval (car clauses) env)) #f)
          (#t (loop (cdr clauses))))))

(define (eval-set! sexpr env)
  (let ((name (string (cadr sexpr)))
        (val (eval (caddr sexpr) env)))
    (let loop ((env env))
      (if (not env)
        (error "undefined" name))
      (let ((p (find (lambda (p) (equal? (car p) name)) (car env))))
       (if p
         (set-cdr! p (list val))
         (loop (cadr env)))))))

(define (call-func sexpr env)
  (let ((f (eval (car sexpr) env))
        (args (map (lambda (s) (eval s env)) (cdr sexpr))))
    (apply f args)))

(define (lookup env sexpr)
  (let ((name (string sexpr))
        (defs (car env))
        (parent (cadr env)))
    (let ((d (find (lambda (d) (equal? (car d) name)) defs)))
      (if d
        (cadr d)
        (if parent (lookup parent name) (error "undefined" name))))))

;;; REPL

(define runtime (string-append runtime "x"))
(define global (make-env runtime))

(define (xlog v)
  (display runtime)
  (display "> ")
  (pretty-print v)
  (newline))

(let repl ()
  (let ((sexpr (read)))
    (if (eof? sexpr)
      "Goodbye!"
      (let ()
        (xlog sexpr)
        (xlog (eval sexpr global))
        (repl)))))

