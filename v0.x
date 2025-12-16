(define next-e
  (let ((x 0))
    (lambda ()
      (set! x (+ x 1))
      x)))

(define next-s
  (let ((x 0))
    (lambda ()
      (set! x (+ x 1))
      x)))

(define constants '())

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

(define (emit sexpr env)
  (let ((e (string-append "%e" (number->string (next-e)))))
        (cond ((number? sexpr)
         (let ()
           (display (string-append "  " e " = call %Val @make_int_val(i64 " (number->string sexpr) ")"))
           (newline)
           e))
          ((boolean? sexpr)
           (let ()
           (display (string-append "  " e " = call %Val @make_bool_val(i1 " (if sexpr "1" "0") ")"))
           (newline)
           e))
          ((string? sexpr)
           (let* ((s (next-s))
                   (sc (string-append "@.str." (number->string s)))
                   (sv (string-append "%s" (number->string s)))
                   (dims (string-append "["
                                       (number->string (+ 1 (string-length sexpr)))
                                       " x i8]")))
             (set! constants (cons (string-append
                                     sc
                                     " = private unnamed_addr constant "
                                     dims
                                     " c\""
                                     sexpr
                                     "\\00\"")
                                   constants))
             ; %s1 = getelementptr [6 x i8], [6 x i8]* @.str.0, i32 0, i32 0
             (display (string-append "  "
                                     sv
                                     " = getelementptr "
                                     dims
                                     ", "
                                     dims
                                     "* "
                                     sc
                                     ", i32 0, i32 0"))
             (newline)
             (display (string-append "  " e " = call %Val @make_str_val(i8* " sv ")"))
             (newline)
             e))
          ((symbol? sexpr)
           (let* ((l (lookup env sexpr))
                  (depth (car l))
                  (offset (cadr l)))
             (display (string-append "; lookup symbol: " (string sexpr)))
             (newline)
             (display (string-append "  "
                                     e
                                     " = call %Val @lookup(%Env %env, i64 "
                                     (number->string depth)
                                     ", i64 "
                                     (number->string offset)
                                     ")"))
             (newline)
             e))
          ((list? sexpr)
           ; what about define?
           ; at the beginning, create a subenv for user defined things
           ; forward pass to allocate the appropriate number of pointers
           ; defines becomes sets on that lookup
           ; call func
           (let ()
             ; recursively emit each part of the operand
             (define fe (emit (car sexpr) env))
             (define args (map (lambda (arg) (emit arg env)) (cdr sexpr)))
             ; create args
             (define i (next-e))
             (display (string-append
                        "  %"
                        (number->string i)
                        " = call i8* @GC_malloc(i64 "
                        (number->string (* 128 (length args)))
                        ")"))
             (newline)
             (define l (next-e))
             (display (string-append 
                        "  %"
                        (number->string l)
                        " = bitcast i8* %"
                        (number->string i)
                        " to %Val*"))
             (newline)
             (define c 0)
             (for-each
               (lambda (arg)
                 (define s (string-append "%" (number->string (next-e))))
                 (display (string-append "  "
                                         s
                                         " = getelementptr %Val, %Val* %"
                                         (number->string l)
                                         ", i64 "
                                         (number->string c)))
                 (newline)
                 (display (string-append "  store %Val " arg ", %Val* " s))
                 (newline)
                 (set! c (+ c 1)))
               args)
             (define a1 (string-append "%a" (number->string (next-e))))
             (display (string-append "  "
                                     a1
                                     "= insertvalue %Args undef, i64 "
                                     (number->string (length args))
                                     ", 1"))
             (newline)
             (define a2 (string-append "%a" (number->string (next-e))))
             (display (string-append "  "
                                     a2
                                     " = insertvalue %Args "
                                     a1
                                     ", %Val* %"
                                     (number->string l)
                                     ", 0"))
             (newline)
             (display (string-append "  "
                                     e
                                     " = call %Val @call_func_val(%Val " 
                                     fe
                                     ", %Args "
                                     a2
                                     ")"))
             (newline)
             e))
        (#t #f))))

