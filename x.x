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

(define bounce-marker (lambda () #f))

(define (bounce? v)
  (and (pair? v) (equal? bounce-marker (car v))))

(define (make-bounce f args)
  (list
    bounce-marker
    (lambda ()
      (apply f args))))

(define (bounce v)
  ((cadr v)))

(define (trampoline v)
  (if (bounce? v)
    (trampoline (bounce v))
    v))

(define (eval sexpr env tail?)
  (let ((v (eval-inner sexpr env tail?)))
    (if tail?
      v
      (trampoline v))))

(define (eval-inner sexpr env tail?)
  (cond ((list? sexpr)    (eval-verb sexpr env tail?))
        ((boolean? sexpr) sexpr)
        ((number? sexpr)  sexpr)
        ((string? sexpr)  sexpr)
        ((symbol? sexpr)  (lookup env sexpr))
        (#t (error "unknown sexpr type" sexpr))))

(define (eval-verb sexpr env tail?)
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
        (#t                                      (call-func sexpr env tail?))))

(define (eval-define sexpr env)
  (if (list? (second sexpr))
    (define-func sexpr env)
    (define-var sexpr env)))

(define (define-var sexpr env)
  (let ((name (string (second sexpr)))
        (val (eval (third sexpr) env #f)))
    (set-car! env (cons (list name val) (car env)))
    val))

(define (define-func sexpr env)
  (let ((name (string (caadr sexpr)))
        (named (map string (cdadr sexpr)))
        (rest #f)
        (body (cddr sexpr)))
    (if (and (> (length named) 1) (equal? (second (reverse named)) "."))
      (let ()
        (set! rest (first (reverse named)))
        (set! named (reverse (cddr (reverse named))))))
    (define f (define-body env #f named rest body))
    (set-car! env (cons (list name f) (car env)))
    f))

(define (define-lambda sexpr env)
  (let* ((args (second sexpr))
         (named (map string (if (list? args) args (list))))
         (rest (if (list? args) #f (string args)))
         (body (cddr sexpr)))
    (if (and (> (length named) 1) (equal? (second (reverse named)) "."))
      (let ()
        (set! rest (first (reverse named)))
        (set! named (reverse (cddr (reverse named))))))
    (define-body env #f named rest body)))

(define (eval-let sexpr env)
  (let* ((let-name (if (list? (second sexpr)) #f (string (second sexpr))))
         (arg-clause (find list? sexpr))
         (argnames (map (lambda (p) (string (car p))) arg-clause))
         (body (if let-name (cdddr sexpr) (cddr sexpr))))
    (let ((f (define-body env let-name argnames #f body)))
      (let arg-loop ((exprs arg-clause)
                     (vals (list)))
        (if (null? exprs)
          (apply f (reverse vals))
          (let* ((arg-env (list (zip argnames (reverse vals)) env))
                 (val (eval (cadar exprs) arg-env #f)))
            (arg-loop (cdr exprs) (cons val vals))))))))

(define (define-body env self named rest body)
  (let ((env (list (list) env)))
    (define (f . args)
      (let ((env (list (bind-args args named rest) env)))
        (let body-loop ((body body) (last #f))
          (if (null? body)
            last
            (body-loop (cdr body)
                       (eval (car body) env (null? (cdr body))))))))
    (if self (set-car! env (list (list self f))))
    f))

(define (bind-args args named rest)
  (append
    (zip (map string named) args)
    (if rest
      (list (list (string rest) (sublist args (length named) (length args))))
      (list))))

(define (eval-if sexpr env)
  (let* ((p (cadr sexpr))
         (t (caddr sexpr))
         (f (if (> (length sexpr) 3) (cadddr sexpr) #f))
         (pv (eval p env #f)))
    (eval (if pv t f) env #t)))

(define (eval-cond sexpr env)
  (let loop ((conds (cdr sexpr)))
    (if (null? conds)
      #f
      (if (eval (caar conds) env #f)
        (eval (cadar conds) env #t)
        (loop (cdr conds))))))

(define (eval-or sexpr env)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) #f)
          ((eval (car clauses) env #f) #t)
          (#t (loop (cdr clauses))))))

(define (eval-and sexpr env)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) #t)
          ((not (eval (car clauses) env #f)) #f)
          (#t (loop (cdr clauses))))))

(define (eval-set! sexpr env)
  (let ((name (string (cadr sexpr)))
        (val (eval (caddr sexpr) env #f)))
    (let loop ((env env))
      (if (not env)
        (error "undefined" name))
      (let ((p (find (lambda (p) (equal? (car p) name)) (car env))))
       (if p
         (set-cdr! p (list val))
         (loop (cadr env)))))))

(define (call-func sexpr env tail?)
  (let ((f (eval (car sexpr) env #f))
        (args (map (lambda (s) (eval s env #f)) (cdr sexpr))))
    (if tail?
      (let ()
        (make-bounce f args))
      (apply f args))))

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
        (let ((v (eval sexpr global #f)))
          (xlog v)
          (repl))))))

