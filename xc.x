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

;;; EMIT

(define (emit sexpr env)
  (cond ((boolean? sexpr)              (emit-bool sexpr env))
        ((number? sexpr)               (emit-number sexpr env))
        ((string? sexpr)               (emit-string sexpr env))
        ((symbol? sexpr)               (emit-lookup-symbol sexpr env))
        ((not (list? sexpr))           (error "invalid sexpr type" sexpr))
        ((equal? (car sexpr) 'define)  (emit-define sexpr env))
        ((equal? (car sexpr) 'set!)    (emit-set sexpr env))
        ((equal? (car sexpr) 'lambda)  (emit-lambda sexpr env))
        ((equal? (car sexpr) 'let)     (emit-let sexpr env))
        ((equal? (car sexpr) 'let*)    (emit-let sexpr env))
        ((equal? (car sexpr) 'if)      (emit-if sexpr env))
        ((equal? (car sexpr) 'cond)    (emit-cond sexpr env))
        ((equal? (car sexpr) 'or)      (emit-or sexpr env))
        ((equal? (car sexpr) 'and)     (emit-and sexpr env))
        (#t (emit-call-func sexpr env))))

(define (emit-bool sexpr env)
  (emit-expr "call %Val @make_bool_val(i1 " (if sexpr 1 0) ")"))

(define (emit-number sexpr env)
  (emit-expr "call %Val @make_int_val(i64 " sexpr ")"))

(define (emit-string sexpr env)
  (define s (emit-const sexpr))
  (emit-expr "call %Val @make_str_val(i8* " s ")"))

(define (emit-lookup-symbol sexpr env)
  (let* ((d (lookup env sexpr))
         (depth (car d))
         (offset (cadr d)))
    (emit-expr "call %Val @lookup(%Env %env, i64 " depth ", i64 " offset ")")))

(define (emit-call-func sexpr env)
  (define fexp (emit (car sexpr) env))
  (define argexps (map (lambda (arg) (emit arg env)) (cdr sexpr)))
  (define args (emit-expr "call %Args @make_args(i64 " (length argexps) ")"))
  (enumerate
    (lambda (i arg) (emit-line "call void @set_arg(%Args " args ", i64 " i ", %Val " arg ")"))
    argexps)
  (emit-expr "call %Val @call_func_val(%Val " fexp ", %Args " args ")"))

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
    ; FIXME: pass named and rest
    (define named (cdadr sexpr))
    (define rest #f)
    (if (equal? (second (reverse named)) (symbol "."))
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
                        (if (equal? (second (reverse named)) (symbol "."))
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
  ; FIXME: pass named and rest
  (define bv (emit-body (cddr sexpr) env self argnames #f #f))
  (define initargs (emit-let-args argdefs env))
  (emit-expr "call %Val @call_func_val(%Val " bv ", %Args " initargs ")"))

(define (emit-let-args argdefs env)
  (define cenv
    (list 
      (enumerate
        (lambda (i argdef)
          (list (car argdef) i))
        argdefs)
      env))
  (define s (string-append "@s" (number->string (next-s))))

  (define pop (push-scope))
  (emit-raw-line "define %Args " s "(%Env %penv) {")
  (emit-line "%env = call %Env @sub_env(%Env %penv, i64 " (length argdefs) ")")

  (define args (emit-expr "call %Args @make_args(i64 " (length argdefs) ")"))

  (enumerate
    (lambda (i argdef)
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
                           (and (list? sexpr) (equal? (car sexpr) 'define)))
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

  ;; FIXME: named args vs. rest args

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
  (let ((pred (cadr sexpr))
        (t (caddr sexpr))
        (f (cadddr sexpr)))
    (define p (emit pred env))
    (define cmp (emit-expr "call i1 @to_i1(%Val " p ")"))
    (define tl (next-l))
    (define fl (next-l))
    (define dl (next-l))
    (emit-line "br i1 " cmp ", label %" tl ", label %" fl)
    (emit-raw-line tl ":")
    (define tv (emit t env))
    (emit-line "br label %" dl)
    (emit-raw-line fl ":")
    (define fv (emit f env))
    (emit-line "br label %" dl)
    (emit-raw-line dl ":")
    (emit-expr "phi %Val [ " tv ", %" tl " ], [ " fv ", %" fl" ]")))

(define (emit-cond sexpr env)
  (define dl (next-l))
  (define bl (next-l))
  (define conds (cdr sexpr))
  (define firstl (next-l))
  (emit-line "br label %" firstl)
  (define phi
    (let loop ((conds conds)
               (current firstl)
               (phi "phi %Val "))
      (if (null? conds)
        phi
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
          (emit-line "br label %" dl)
          (define rest (cdr conds))
          (loop
            rest
            next
            (string-append phi "[ " v ", %" tl " ], " ))))))

  (emit-raw-line bl ":")
  (define bv (emit-expr "call %Val @make_bool_val(i1 0)"))
  (emit-line "br label %" dl)

  (set! phi (string-append phi "[ " bv ", %" bl " ]"))

  (emit-raw-line dl ":")
  (emit-expr phi))

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
  (emit-raw-line "define i32 @main() {" )
  (emit-line "%env = call %Env @make_global_env()")
  (define m (emit-body all env #f '() #f #t))
  (define args (emit-expr "insertvalue %Args undef, i64 0, 1"))
  (emit-line "call %Val @call_func_val(%Val " m ", %Args " args ")")
  (emit-line "ret i32 0")
  (emit-raw-line "}")
  (set! scopes (cons (reverse scope) scopes)))

(define (lookup env sym)
  (let loop ((env env)
             (depth 0))
    (if (not env)
      (error "undefined symbol" sym)
      (let* ((vals (car env))
             (def (assoc sym vals)))
        (if def
          (list depth (cadr def))
          (loop (cadr env) (+ 1 depth)))))))

;;; ASSEMBLE

(define constants '())
(define scope '())
(define scopes '())

(define s 0)
(define (next-s)
  (set! s (+ s 1))
  s)

; returns pop
(define (push-scope)
  (define last scope)
  (set! scope '())
  (lambda ()
    (set! scopes (cons (reverse scope) scopes))
    (set! scope last)))

(define c 0)
(define (emit-const s)
  (set! c (+ c 1))
  (define dims (string-append "["
                              (number->string (+ 1 (string-length s)))
                              " x i8]"))
  (define cv (string-append "@.str." (number->string c)))
  (set! constants (cons (list cv " = private unnamed_addr constant " dims "c \"" s "\\00\"") constants))
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
          (display (apply string-append (map
                                          (lambda (x) (if (number? x) (number->string x) x)) line)))
          (newline))
        scope)
      (newline))
    (append (list constants) scopes)))

;;; MAIN

(define global (list
                 (list
                   (list (symbol "list") 0)
                   (list (symbol "+") 1))
                  #f))


(let ((all (let loop ((all '()))
  (let ((sexpr (read)))
    (if (eof? sexpr)
      (reverse all)
      (loop (cons sexpr all)))))))
  (emit-main all global)
  (print-file))
