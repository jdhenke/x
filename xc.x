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
        ((equal? (car sexpr) 'define)  (emit-set sexpr env))
        ((equal? (car sexpr) 'set!)    (emit-set sexpr env))
        ;; ADD special forms!
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
    (emit-define-set sexpr env)))

(define (emit-set sexpr env)
  (let* ((d (lookup env (cadr sexpr)))
         (depth (car d))
         (offset (cadr d)))
    (define e (emit (caddr sexpr) env))
    (emit-line "call void @set(%Env %env, i64 " depth ", i64 " offset ", %Val " e ")")
    e))

(define (emit-define-func) (error "not implemented"))
;(define (emit-lambda))
;(define (emit-let))

(define (emit-body body env self args print?)
  ; called from containing scope
  ; assemble closure env to include
  ; - parent (^env)
  ; - TODO: self (defined in ^env already, or let loop in new one)
  ; - TODO: args 
  ; - defs (forward pass)

  (define defs (enumerate
                 (lambda (i sexpr )
                   (list (if (list? (cadr sexpr)) (caadr sexpr) (cadr sexpr))
                         i))
                 (filter (lambda (sexpr)
                           (and (list? sexpr) (equal? (car sexpr) 'define)))
                         body)))

  (define cenv (list defs env))
  (define ev (emit-line "%env = call %Env @sub_env(%Env %penv, i64 " (length cenv) ")"))
  ; 
  ; push scope
  ;   ; now outside of calling scope
  ;   function header {
  ;     assign args to env
  ;     for each, switch on type, emit type
    (for-each
      (lambda (sexpr)
        (define e (emit sexpr cenv))
        (if print? (emit-line "call void @println(%Val " e ")")))
      body)
  ;     if print, println %e#


  ;     return last e
  ;   }
  ; pop scope
  ;
  ; ; back in calling scope
  ; 
  ; emit-line %e# = <create func val>
  ; if self, store in self ptr
  ; returns %e# 
  )

;(define (emit-if))
;(define (emit-cond))
;(define (emit-or))
;(define (emit-and))

(define (emit-main all env)
  (emit-raw-line "define i32 @main() {" )
  (emit-line "%penv = call %Env @make_global_env()")
  (emit-body all env #f '() #t)
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

; returns pop
;(define (push-scope)
;  (lambda ()))

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
