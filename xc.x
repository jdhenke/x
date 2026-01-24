;;; EMIT

(define (emit sexpr env)
  (cond ((boolean? sexpr)              (emit-bool sexpr env))
        ((number? sexpr)               (emit-number sexpr env))
        ((string? sexpr)               (emit-string sexpr env))
        ((symbol? sexpr)               (emit-lookup-symbol sexpr env))
        ((not (list? sexpr))           (error "invalid sexpr type" sexpr))
        ((equal? (car sexpr) (symbol "define"))  (emit-define sexpr env))
        ((equal? (car sexpr) (symbol "quote"))   (emit-quote (cadr sexpr) env))
        ((equal? (car sexpr) (symbol "set!"))    (emit-set sexpr env))
        ((equal? (car sexpr) (symbol "lambda"))  (emit-lambda sexpr env))
        ((equal? (car sexpr) (symbol "let"))     (emit-let sexpr env))
        ((equal? (car sexpr) (symbol "let*"))    (emit-let sexpr env))
        ((equal? (car sexpr) (symbol "if"))      (emit-if sexpr env))
        ((equal? (car sexpr) (symbol "cond"))    (emit-cond sexpr env))
        ((equal? (car sexpr) (symbol "or"))      (emit-or sexpr env))
        ((equal? (car sexpr) (symbol "and"))     (emit-and sexpr env))
        (#t (emit-call-func sexpr env))))

(define (emit-bool sexpr env)
  (emit-expr "call %Val @make_bool_val(i1 " (if sexpr 1 0) ")"))

(define (emit-number sexpr env)
  (emit-expr "call %Val @make_int_val(i64 " sexpr ")"))

(define (emit-string sexpr env)
  (define s (emit-const sexpr))
  (emit-expr "call %Val @make_str_val(i64 " (string-length sexpr) ", i8* " s ")"))

(define (emit-lookup-symbol sexpr env)
  (let* ((d (lookup env sexpr))
         (depth (car d))
         (offset (cadr d)))
    (emit-expr "call %Val @lookup(%Env %env, i64 " depth ", i64 " offset ")")))

(define (emit-quote sexpr env)
  (cond
    ((boolean? sexpr) (emit-bool sexpr env))
    ((number? sexpr)  (emit-number sexpr env))
    ((string? sexpr)  (emit-string sexpr env))
    ((symbol? sexpr)  (emit-expr "call %Val @make_sym_val(i64 " (string-length (string sexpr)) ", i8* " (emit-const (string sexpr)) ")"))
    ((list? sexpr)
     (let ((es (map (lambda (s) (emit-quote s env)) sexpr)))
       (define args (emit-expr "call %Args @make_args(i64 " (length es) ")"))
       (enumerate
         (lambda (i e) (emit-line "call void @set_arg(%Args " args ", i64 " i ", %Val " e ")"))
         es)
       (emit-expr "tail call %Val @call_list(%Env %env, %Args " args ")")))
    (#t (error "unknown quote type"))))

(define (emit-call-func sexpr env)
  (define fexp (emit (car sexpr) env))
  (define argexps (map (lambda (arg) (emit arg env)) (cdr sexpr)))
  (define args (emit-expr "call %Args @make_args(i64 " (length argexps) ")"))
  (enumerate
    (lambda (i arg) (emit-line "call void @set_arg(%Args " args ", i64 " i ", %Val " arg ")"))
    argexps)
  (emit-expr "tail call %Val @call_func_val(%Val " fexp ", %Args " args ")"))

(define (emit-define sexpr env)
  (if (list? (cadr sexpr))
    (emit-define-func sexpr env)
    (emit-set sexpr env)))

(define (emit-set sexpr env)
  (let* ((d (lookup env (cadr sexpr)))
         (depth (car d))
         (offset (cadr d)))
    (define e (emit (caddr sexpr) env))
    (emit-line "call void @set(%Env %env, i64 " depth ", i64 " offset ", %Val " e ")")
    e))

(define (emit-define-func sexpr env)
  (let* ((d (lookup env (caadr sexpr)))
         (depth (car d))
         (offset (cadr d)))
    (define named (cdadr sexpr))
    (define rest #f)
    (if (and (>= (length named) 2) (equal? (second (reverse named)) (symbol ".")))
      (let ()
        (set! rest (first (reverse named)))
        (set! named (reverse (cddr (reverse named))))))
    (define e (emit-body (cddr sexpr) env #f named rest #f))
    (emit-line "call void @set(%Env %env, i64 " depth ", i64 " offset ", %Val " e ")")
    e))

(define (emit-lambda sexpr env)
  (define vars (cond ((list? (second sexpr))
                      (let ()
                        (define named (cadr sexpr))
                        (define rest #f)
                        (if (and (>= (length named) 2) (equal? (second (reverse named)) (symbol ".")))
                          (let ()
                            (set! rest (first (reverse named)))
                            (set! named (reverse (cddr (reverse named))))))
                        (list named rest)))
                     (#t (list (list) (second sexpr)))))
  (define named (car vars))
  (define rest (cadr vars))
  (emit-body (cddr sexpr) env #f named rest #f))

(define (emit-let sexpr env)
  (define self (if (list? (cadr sexpr)) #f (cadr sexpr)))
  (define argdefs ((if self caddr cadr) sexpr))
  (define argnames (map car argdefs))
  (define bv (emit-body ((if self cdddr cddr) sexpr) env self argnames #f #f))
  (define initargs (emit-let-args argdefs env))
  (emit-expr "tail call %Val @call_func_val(%Val " bv ", %Args " initargs ")"))

(define (emit-let-args argdefs env)
  (define s (string-append "@s" (number->string (next-s))))

  (define pop (push-scope))
  (emit-raw-line "define %Args " s "(%Env %penv) {")
  (emit-line "%env = call %Env @sub_env(%Env %penv, i64 " (length argdefs) ")")

  (define args (emit-expr "call %Args @make_args(i64 " (length argdefs) ")"))

  (enumerate
    (lambda (i argdef)
      ; incrementally add to cenv as each argdef is defined
      (define cenv (list 
        (filter
          (lambda (e) (< (cadr e) i))
          (enumerate
            (lambda (i argdef)
              (list (car argdef) i))
            argdefs))
        env))
      (define argv (emit (cadr argdef) cenv))
      (emit-line "call void @set(%Env %env, i64 0, i64 " i ", %Val " argv ")")
      (emit-line "call void @set_arg(%Args " args ", i64 " i ", %Val " argv ")"))
    argdefs)

  (emit-line "ret %Args " args)
  (emit-raw-line "}")
  (pop)

  (emit-expr "call %Args " s "(%Env %env)"))

(define (emit-body body env self named rest print?)
  (define argdefs (enumerate
                    (lambda (i arg) (list arg i))
                    named))

  (if rest (set! argdefs (append argdefs (list (list rest (length argdefs))))) )

  (define bodydefs (enumerate
                 (lambda (i sexpr )
                   (list (if (list? (cadr sexpr)) (caadr sexpr) (cadr sexpr))
                         (+ (length argdefs) i)))
                 (filter (lambda (sexpr)
                           (and (list? sexpr) (equal? (car sexpr) (symbol "define"))))
                         body)))

  (define local (append argdefs bodydefs ))

  ; must be updated with f val when constructed
  (define penv (if self (list (list (list self 0)) env) env))
  (define ev
    (if self 
      (emit-expr "call %Env @sub_env(%Env %env, i64 1)")
      "%env"))

  (define cenv (list local penv))
  (define n (length local))

  ;;; new scope

  (define pop (push-scope))

  (define s (string-append "@s" (number->string (next-s))))
  (emit-raw-line "define %Val " s "(%Env %penv, %Args %args) {")

  (emit-line "%env = call %Env @sub_env(%Env %penv, i64 " n ")")

  (enumerate
    (lambda (i arg) ; is is correct but argdefs come first
        (define a (emit-expr "call %Val @get_arg(%Args %args, i64 " i ")"))
        (emit-line "call void @set(%Env %env, i64 0, i64 " i ", %Val " a ")"))
    named)

  (if rest
    (emit-line "call void @set_rest(%Env %env, %Args %args, i64 " (length named) ")"))

  (define last #f)
  (for-each
    (lambda (sexpr)
      (define e (emit sexpr cenv))
      (set! last e)
      (if print? (emit-line "call void @println(%Val " e ")")))
    body)

  (emit-line "ret %Val " last)
  (emit-raw-line "}")

  (pop)

  ;;; orig scope

  (define f (emit-expr "call %Val @to_func_val(%Val(%Env, %Args)* " s ", %Env " ev ")"))

  ; must match env construction before
  (if self (emit-line "call void @set(%Env " ev ", i64 0, i64 0, %Val " f ")"))

  f)

(define (emit-if sexpr env)
  (emit-in-func
    (lambda ()
      (emit-if-helper sexpr env))))

(define (emit-if-helper sexpr env)
  (let ((pred (cadr sexpr))
        (t (caddr sexpr))
        (f (if (> (length sexpr) 3) (cadddr sexpr) #f)))
    (define p (emit pred env))
    (define cmp (emit-expr "call i1 @to_i1(%Val " p ")"))
    (define tl (next-l))
    (define fl (next-l))
    (emit-line "br i1 " cmp ", label %" tl ", label %" fl)

    (emit-raw-line tl ":")
    (define tv (emit t env))
    (emit-line "ret %Val " tv)

    (emit-raw-line fl ":")
    (emit f env))) ; emit-in-func adds return in this case

(define (emit-in-func f)
    (define pop (push-scope))
    (define s (string-append "@s" (number->string (next-s))))
    (emit-raw-line "define %Val " s "(%Env %env) {")

    (define out (f))

    (emit-line "ret %Val " out)
    (emit-raw-line "}")
    (pop)

    (emit-expr "tail call %Val " s "(%Env %env)"))

(define (emit-cond sexpr env)
  (emit-in-func
    (lambda ()
      (emit-cond-helper sexpr env))))

(define (emit-cond-helper sexpr env)
  (define bl (next-l))
  (define conds (cdr sexpr))
  (define firstl (next-l))
  (emit-line "br label %" firstl)
  (let loop ((conds conds)
             (current firstl))
    (if (null? conds)
      #f
      (let ()
        (emit-raw-line current ":")
        (define cond (car conds))
        (define pexpr (car cond))
        (define texpr (cadr cond))
        (define p (emit pexpr env))
        (define tl (next-l))
        (define cmp (emit-expr "call i1 @to_i1(%Val " p ")"))
        (define next (if (= (length conds) 1) bl (next-l)))
        (emit-line "br i1 " cmp ", label %" tl ", label %" next)
        (emit-raw-line tl ":")
        (define v (emit texpr env))
        (emit-line "ret %Val " v)
        (loop
          (cdr conds)
          next))))
  (emit-raw-line bl ":")
  (emit-expr "tail call %Val @make_bool_val(i1 0)")) ; emit-in-func adds retun

(define (emit-or sexpr env)
  (define clauses (cdr sexpr))
  (define tl (next-l))
  (define fl (next-l))
  (define firstl (next-l))
  (emit-line "br label %" firstl)
  (let loop ((clauses clauses)
             (current firstl))
    (if (null? clauses)
      #f
      (let ()
        (define clause (car clauses))
        (emit-raw-line current ":")
        (define e (emit clause env))
        (define b (emit-expr "call i1 @to_i1(%Val " e ")"))
        (define next (if (= (length clauses) 1) fl (next-l)))
        (emit-line "br i1 " b ", label %" tl ", label %" next)
        (loop (cdr clauses)
              next))))
  (define dl (next-l))
  (emit-raw-line tl ":")
  (define tv (emit-expr "call %Val @make_bool_val(i1 1)"))
  (emit-line "br label %" dl)
  (emit-raw-line fl ":")
  (define fv (emit-expr "call %Val @make_bool_val(i1 0)"))
  (emit-line "br label %" dl)
  (emit-raw-line dl ":")
  (emit-expr "phi %Val [ " tv ", %" tl " ], [ " fv ", %" fl " ]"))

(define (emit-and sexpr env)
  (define clauses (cdr sexpr))
  (define tl (next-l))
  (define fl (next-l))
  (define firstl (next-l))
  (emit-line "br label %" firstl)
  (let loop ((clauses clauses)
             (current firstl))
    (if (null? clauses)
      #f
      (let ()
        (define clause (car clauses))
        (emit-raw-line current ":")
        (define e (emit clause env))
        (define b (emit-expr "call i1 @to_i1(%Val " e ")"))
        (define next (if (= (length clauses) 1) tl (next-l)))
        (emit-line "br i1 " b ", label %" next ", label %" fl)
        (loop (cdr clauses)
              next))))
  (define dl (next-l))
  (emit-raw-line tl ":")
  (define tv (emit-expr "call %Val @make_bool_val(i1 1)"))
  (emit-line "br label %" dl)
  (emit-raw-line fl ":")
  (define fv (emit-expr "call %Val @make_bool_val(i1 0)"))
  (emit-line "br label %" dl)
  (emit-raw-line dl ":")
  (emit-expr "phi %Val [ " tv ", %" tl " ], [ " fv ", %" fl " ]"))

(define (emit-main all env)
  (emit-raw-line "define i32 @main(i32 %argc, i8** %argv) {" )
  (emit-line "call void @seed_random()")
  (emit-line "%env = call %Env @make_global_env(i32 %argc, i8** %argv)")
  (define m (emit-body all env #f (list) #f #f))
  (define args (emit-expr "insertvalue %Args undef, i64 0, 1"))
  (emit-line "call %Val @call_func_val(%Val " m ", %Args " args ")")
  (emit-line "ret i32 0")
  (emit-raw-line "}")
  (set! scopes (cons (reverse scope) scopes)))

(define (lookup env sym)
  (let loop ((env env)
             (depth 0))
    (if (not env)
      (error "xc: undefined symbol: " (string-append "[" sym "]"))
      (let* ((vals (car env))
             (def (assoc sym vals)))
        (if def
          (list depth (cadr def))
          (loop (cadr env) (+ 1 depth)))))))

;;; ASSEMBLE

(define constants (list))
(define scope (list))
(define scopes (list))

(define s 0)
(define (next-s)
  (set! s (+ s 1))
  s)

(define (escape s)
  (let loop ((cs (string-list s))
             (out (list)))
    (if (null? cs)
      (apply string-append (reverse out))
      (let ()
        (define nout
          (let ((c (car cs)))
            (cond ((equal? c "\"") (cons "2" (cons "2" (cons "\\" out))))
                  ((equal? c "\\") (cons "C" (cons "5" (cons "\\" out))))
                  ((equal? c "\n") (cons "A" (cons "0" (cons "\\" out))))
                  (#t (cons c out)))))
        (loop (cdr cs) nout)))))

; returns pop
(define (push-scope)
  (define last scope)
  (set! scope (list))
  (lambda ()
    (set! scopes (cons (reverse scope) scopes))
    (set! scope last)))

(define c 0)
(define (emit-const s)
  (set! c (+ c 1))
  (define esc (escape s))
  (define dims (string-append "["
                              (number->string (string-length s))
                              " x i8]"))
  (define cv (string-append "@.str." (number->string c)))
  (set! constants (cons (list cv " = private unnamed_addr constant " dims " c\"" esc "\"") constants))
  cv)

(define (emit-raw-line . args)
  (set! scope (cons args scope)))

(define (emit-line . args)
  (set! scope (cons (cons "  " args) scope)))

(define e 0)
(define (emit-expr . args)
  (set! e (+ e 1))
  (let ((ev (string-append "%e" (number->string e))))
    (apply emit-line (append (list ev " = ") args))
    ev))

(define l 0)
(define (next-l)
  (set! l (+ l 1))
  (string-append "l" (number->string l)))
  
(define (print-file)
  (for-each
    (lambda (scope)
      (for-each
        (lambda (line)
          (print (apply string-append
                        (map (lambda (x) (if (number? x) (number->string x) x)) line)))
          (newline))
        scope)
      (newline))
    (append (list constants) scopes)))

;;; MAIN

(define global (list
                 (list
                   (list (symbol "sys/exit") 0)
                   (list (symbol "list") 1)
                   (list (symbol "+") 2)
                   (list (symbol "apply") 3)
                   (list (symbol "sys/open") 4)
                   (list (symbol "sys/close") 5)
                   (list (symbol "sys/dup") 37)
                   (list (symbol "sys/dup2") 38)
                   (list (symbol "sys/write") 6)
                   (list (symbol "sys/read") 8)
                   (list (symbol "sys/fork") 9)
                   (list (symbol "eq?") 10)
                   (list (symbol "equal?") 10)
                   (list (symbol "sys/wait") 11)
                   (list (symbol "sys/execve") 12)
                   (list (symbol "string-append") 13)
                   (list (symbol "car") 14)
                   (list (symbol "cdr") 15)
                   (list (symbol "cons") 16)
                   (list (symbol "null?") 17)
                   (list (symbol "<") 18)
                   (list (symbol "-") 19)
                   (list (symbol "symbol") 20)
                   (list (symbol "boolean?") 21)
                   (list (symbol "number?") 22)
                   (list (symbol "string?") 23)
                   (list (symbol "list?") 24)
                   (list (symbol "symbol?") 25)
                   (list (symbol "function?") 26)
                   (list (symbol "length") 27)
                   (list (symbol "string-length") 28)
                   (list (symbol "string-list") 29)
                   (list (symbol "set-car!") 30)
                   (list (symbol "string") 31)
                   (list (symbol "set-cdr!") 32)
                   (list (symbol "modulo") 33)
                   (list (symbol "*") 34)
                   (list (symbol "/") 35)
                   (list (symbol "command-line") 39)
                   (list (symbol "runtime") 40)
                   (list (symbol "random") 41)
                   (list (symbol "string<?") 42)
                   (list (symbol "substring") 43)
                   )
                  #f))

(define (collect-sexprs)
  (let loop ((all (list)))
    (let ((sexpr (read)))
      (if (eof? sexpr)
        (reverse all)
        (loop (cons sexpr all))))))

(define (run exe . args)
  (let ((code (run-synchronous-subprocess exe args)))
    (print "") ; flush stdout
    (if (not (= code 0))
      (let ()
        (println (string-append exe " error"))
        (sys/exit code)))))

(define (main)
  (let ((out
        (let loop ((args (command-line)))
          (if (null? args)
            "/tmp/exe"
            (if (equal? (car args) "-o")
              (cadr args)
              (loop (cdr args))))))
      (ll "/tmp/out.ll"))
    (let ((main-sexprs (collect-sexprs))
          (std (read-std)))
      (let ((all (append std main-sexprs)))
        (emit-main all global)
        (with-output-to-file ll
          (lambda ()
            (print-file)))
        (run "/bin/sh" "-c" (string-append "cat xc.ll " ll " > /tmp/both.ll"))
        (run "/usr/bin/arch" "-arm64" "clang" "-O1" "-Wno-override-module" "-lgc" "/tmp/both.ll" "-o" out)
        (if (equal? out "/tmp/exe")
          (run out)
          (sys/exit 0))))))

(main)
