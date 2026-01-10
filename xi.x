;;; EVAL

(define (eval sexpr env k)
  (cond ((list? sexpr)    (eval-verb sexpr env k))
        ((boolean? sexpr) (k sexpr))
        ((number? sexpr)  (k sexpr))
        ((string? sexpr)  (k sexpr))
        ((symbol? sexpr)  (k (lookup env sexpr)))
        (#t (error "unknown sexpr type" sexpr))))

(define (eval-verb sexpr env k)
  (cond ((null? sexpr) sexpr)
        ((equal? (car sexpr) (symbol "define"))  (eval-define sexpr env k))
        ((equal? (car sexpr) (symbol "set!"))    (eval-set! sexpr env k))
        ((equal? (car sexpr) (symbol "lambda"))  (define-lambda sexpr env k))
        ((equal? (car sexpr) (symbol "apply"))   (eval-apply sexpr env k))
        ((equal? (car sexpr) (symbol "let"))     (eval-let sexpr env k))
        ((equal? (car sexpr) (symbol "let*"))    (eval-let sexpr env k))
        ((equal? (car sexpr) (symbol "if"))      (eval-if sexpr env k))
        ((equal? (car sexpr) (symbol "cond"))    (eval-cond sexpr env k))
        ((equal? (car sexpr) (symbol "or"))      (eval-or sexpr env k))
        ((equal? (car sexpr) (symbol "and"))     (eval-and sexpr env k))
        (#t                                      (call-func sexpr env k))))

(define (eval-define sexpr env k)
  (if (list? (second sexpr))
    (define-func sexpr env k)
    (define-var sexpr env k)))

(define (define-var sexpr env k)
  (let ((name (second sexpr)))
    (eval (third sexpr) env 
          (lambda (val)
            (set-car! env (cons (list name val) (car env)))
            (k val)))))

(define (define-func sexpr env k)
  (let ((name (caadr sexpr))
        (named (cdadr sexpr))
        (rest #f)
        (body (cddr sexpr)))
    (if (and (> (length named) 1) (equal? (second (reverse named)) (symbol ".")))
      (let ()
        (set! rest (first (reverse named)))
        (set! named (reverse (cddr (reverse named))))))
    (define f (define-body env #f named rest body))
    (set-car! env (cons (list name f) (car env)))
    (k f)))

(define (define-lambda sexpr env k)
  (let* ((args (second sexpr))
         (named (if (list? args) args (list)))
         (rest (if (list? args) #f args))
         (body (cddr sexpr)))
    (if (and (> (length named) 1) (equal? (second (reverse named)) (symbol ".")))
      (let ()
        (set! rest (first (reverse named)))
        (set! named (reverse (cddr (reverse named))))))
    (k (define-body env #f named rest body))))

(define (eval-apply sexpr env k)
  (eval (cadr sexpr) env
        (lambda (f)
          (eval (caddr sexpr) env
                (lambda (args)
                  (apply f (cons k args)))))))

(define (eval-let sexpr env k)
  (let* ((let-name (if (list? (second sexpr)) #f (second sexpr)))
         (arg-clause (find list? sexpr))
         (argnames (map (lambda (p) (car p)) arg-clause))
         (body (if let-name (cdddr sexpr) (cddr sexpr))))
    (let ((f (define-body env let-name argnames #f body)))
      (let arg-loop ((exprs arg-clause)
                     (vals (list)))
        (if (null? exprs)
          (apply f (cons k (reverse vals)))
          (let* ((arg-env (list (zip argnames (reverse vals)) env)))
            (eval (cadar exprs) arg-env
                  (lambda (val)
                    (arg-loop (cdr exprs) (cons val vals))))))))))

(define (define-body env self named rest body)
  (let ((env (list (list) env)))
    (define (f k . args)
      (let ((env (list (bind-args args named rest) env)))
        (define (body-loop-cps body)
          (cond ((null? body) (k #f))
                ((null? (cdr body)) (eval (car body) env k))
                (#t (let ()
                      (eval (car body) env (lambda (v) #f)) ; discard
                      (body-loop-cps (cdr body))))))
        (body-loop-cps body)))
    (if self (set-car! env (list (list self f))))
    f))

(define (bind-args args named rest)
  (append
    (zip named args)
    (if rest
      (list (list rest (sublist args (length named) (length args))))
      (list))))

(define (eval-if sexpr env k)
  (let* ((p (cadr sexpr))
         (t (caddr sexpr))
         (f (if (> (length sexpr) 3) (cadddr sexpr) #f)))
    (eval p env
          (lambda (pv)
            (eval (if pv t f) env k)))))

(define (eval-cond sexpr env k)
  (let loop ((conds (cdr sexpr)))
    (if (null? conds)
      (k #f)
      (eval (caar conds) env
            (lambda (cv)
              (if cv
                (eval (cadar conds) env k)
                (loop (cdr conds))))))))

(define (eval-or sexpr env k)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) (k #f))
          (#t (eval (car clauses) env
                    (lambda (cv)
                      (if cv
                        (k #t)
                        (loop (cdr clauses)))))))))

(define (eval-and sexpr env k)
  (let loop ((clauses (cdr sexpr)))
    (cond ((null? clauses) (k #t))
          (#t (eval (car clauses) env
                    (lambda (cv)
                      (if cv
                        (loop (cdr clauses))
                        (k #f))))))))

(define (eval-set! sexpr env k)
  (let ((name (cadr sexpr)))
    (eval (caddr sexpr) env
          (lambda (val)
            (let loop ((env env))
              (if (not env)
                (error "set: undefined: " name))
              (let ((p (assq name (car env))))
               (if p
                 (let ()
                   (set-cdr! p (list val))
                   (k val))
                 (loop (cadr env)))))))))

(define (call-func sexpr env k)
  (eval (car sexpr) env
        (lambda (f)
          (define args (list))
          (for-each (lambda (s) (eval s env
                                      (lambda (argv)
                                        (set! args (append args (list argv))))))
                    (cdr sexpr))
          (apply f (cons k args)))))

(define (lookup env name)
  (let ((defs (car env))
        (parent (cadr env)))
    (let ((d (assq name defs)))
      (if d
        (cadr d)
        (if parent (lookup parent name) (error "undefined" runtime name))))))

;;; REPL

;;; FIXME! ; why can't I redefined runtime??
(set! runtime (string-append runtime "i"))

(define (native-to-cps n f)
  (lambda (k . args)
    (k (apply f args))))

(define (make-env runtime)
  (list
    (map
      (lambda (nl)
        (list (car nl) (native-to-cps (car nl) (cadr nl))))
      (list
        (list (symbol "*") *)
        (list (symbol "+") +)
        (list (symbol "+") +)
        (list (symbol "-") -)
        (list (symbol "/") /)
        (list (symbol "<") <)
        (list (symbol "<=") <=)
        (list (symbol "=") =)
        (list (symbol ">") >)
        (list (symbol ">=") >=)
        (list (symbol "append") append)
        (list (symbol "assq") assq)
        (list (symbol "boolean?") boolean?)
        (list (symbol "car") car)
        (list (symbol "cdr") cdr)
        (list (symbol "cons") cons)
        (list (symbol "command-line") command-line)
        (list (symbol "eq?") eq?)
        (list (symbol "equal?") equal?)
        (list (symbol "function?") function?)
        (list (symbol "length") length)
        (list (symbol "list") list)
        (list (symbol "list?") list?)
        (list (symbol "modulo") modulo)
        (list (symbol "null?") null?)
        (list (symbol "number?") number?)
        (list (symbol "pair?") pair?)
        (list (symbol "runtime") runtime)
        (list (symbol "set-car!") set-car!)
        (list (symbol "set-cdr!") set-cdr!)
        (list (symbol "string") string)
        (list (symbol "string-append") string-append)
        (list (symbol "string-length") string-length)
        (list (symbol "string-list") string-list)
        (list (symbol "string?") string?)
        (list (symbol "symbol") symbol)
        (list (symbol "symbol?") symbol?)
        (list (symbol "sys/exit") sys/exit)
        (list (symbol "sys/read") sys/read)
        (list (symbol "sys/write") sys/write)
        (list (symbol "with-input-from-file") with-input-from-file)
        (list (symbol "with-output-to-file") with-output-to-file)
        (list (symbol "run-synchronous-subprocess") run-synchronous-subprocess)
      ))
    #f))

(define global (make-env runtime))

(define (repl)
  (let loop ()
    (let ((sexpr (read)))
      (if (eof? sexpr)
        #f
        (let ()
          (eval sexpr global (lambda (v) v))
          (loop))))))

(define (main)
  (let loop ((std (read-std)))
    (if (null? std)
      #f
      (let ()
        (eval (car std) global (lambda (v) v))
        (loop (cdr std)))))
  (repl))

(main)
