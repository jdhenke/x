;;; OUTPUT

(define (print x)
  (cond
    ((string? x)    (sys/write 1 x (string-length x)))
    ((symbol? x)    (print (string x)))
    ((boolean? x)   (print (if x "#t" "#f")))
    ((number? x)    (print (number->string x)))
    ((function? x)  (print "λ"))
    ((list? x)
     (let ()
       (print "(")
       (let loop ((l x))
         (if (null? l)
           (print ")")
           (let ()
             (print (car l))
             (if (not (null? (cdr l)))
               (print " "))
             (loop (cdr l)))))))))

(define (println x)
  (print x)
  (print "\n"))

(define (newline)
  (print "\n"))

(define error (lambda args
  (println "ERROR")
  (map println args)
  (sys/exit 1)))

;;; BOOLEANS

(define (not x)
  (if x #f #t))

;;; INTEGERS

(define = equal?)

(define (> a b) (< b a))

(define (>= a b)
  (< b (+ a 1)))

(define (<= a b) (>= b a))

;;; LISTS

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (fourth l) (cadddr l))
(define (last l) (first (reverse l)))

(define (reverse l)
  (let loop ((l l) (t (list)))
    (if (null? l)
      t
      (loop (cdr l) (cons (car l) t)))))

(define append (lambda args
  (let arglp ((args args)
               (out (list)))
    (if (null? args)
      (reverse out)
      (arglp (cdr args)
             (let itemlp ((l (car args))
                          (out out))
               (if (null? l)
                 out
                 (itemlp (cdr l) (cons (car l) out)))))))))

(define (find f l)
  (let loop ((l l))
    (if (null? l)
      #f
      (if (f (car l))
        (car l)
        (loop (cdr l))))))

(define (map f l)
  (let loop ((l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (cdr l) (cons (f (car l)) out)))))

(define (enumerate f l)
  (let loop ((i 0)
             (l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (+ i 1) (cdr l) (cons (f i (car l)) out)))))

(define zip (lambda ls
  (let loop ((ls ls)
             (out (list)))
    (if (find null? ls)
      (reverse out)
      (loop (map cdr ls) (cons (map car ls) out))))))

(define (filter f l)
  (let loop ((l l)
             (out (list)))
    (if (null? l)
      (reverse out)
      (loop (cdr l)
            (if (f (car l))
              (cons (car l) out)
              out)))))

(define for-each map)

(define (assoc k l)
  (find (lambda (r) (equal? (car r) k)) l))

(define (sublist l i j)
  (let loop ((l l)
        (i i)
        (j j)
        (out (list)))
    (cond ((> i 0) (loop (cdr l) (- i 1) (- j 1) out))
          ((> j 0) (loop (cdr l) 0 (- j 1) (cons (car l) out)))
          (#t (reverse out)))))

(define assq
  (if (function? assq)
    assq
    (lambda (k l) (find (lambda (r) (eq? (car r) k)) l))))

;;; STRINGS

(define n-to-s
  (list (list 0 "0")
        (list 1 "1")
        (list 2 "2")
        (list 3 "3")
        (list 4 "4")
        (list 5 "5")
        (list 6 "6")
        (list 7 "7")
        (list 8 "8")
        (list 9 "9")))

(define s-to-n
  (list (list "0" 0)
        (list "1" 1)
        (list "2" 2)
        (list "3" 3)
        (list "4" 4)
        (list "5" 5)
        (list "6" 6)
        (list "7" 7)
        (list "8" 8)
        (list "9" 9)))

(define (number->string x)
  (define out (list))
  (if (< x 0)
    (let () (set! x (- 0 x)) (set! out (cons "-" out))))
  (let loop ((x x) (out out))
    (if (> x 9)
      (let ((r (modulo x 10)))
        (loop (/ x 10) (cons (cadr (assoc r n-to-s)) out)))
      (apply string-append (cons (cadr (assoc x n-to-s)) out)))))

(define (string->number s)
  (let loop ((cs (string-list s)) (out 0))
    (if (null? cs)
      out
      (loop (cdr cs) (+ (* out 10) (cadr (assoc (car cs) s-to-n)))))))

(define (string-number? s)
  (and
    (> (string-length s) 0)
    (let loop ((l (string-list s)))
      (if (null? l)
        #t
        (if (assoc (car l) s-to-n)
          (loop (cdr l))
          #f)))))

;;; FUNCTIONS

(define curry (lambda args
  (define f (car args))
  (define largs (cdr args))
  (lambda rargs
    (apply f (append largs rargs)))))


;;; SYSCALL

(define with-input-from-file
  (if (function? with-input-from-file)
    (let ((orig with-input-from-file))
      (lambda (p f)
        (define old _peek-c)
        (set! _peek-c #f)
        (define out (orig p f)) ; call
        (set! _peek-c old)
        out))
    (lambda (p f)
      (let ((fd (sys/open p 0)))
        (let ((original (sys/dup 0)))
          (sys/dup2 fd 0)
          (sys/close fd)
          (define old _peek-c)
          (set! _peek-c #f)
          (define out (f)) ; call
          (set! _peek-c old)
          (sys/dup2 original 0)
          (sys/close original)
          out)))))

(define with-output-to-file
  (if (function? with-output-to-file)
    with-output-to-file
    (lambda (p f)
      (let ((fd (sys/open p 1537 438)))
        (let ((original (sys/dup 1)))
          (sys/dup2 fd 1)
          (sys/close fd)
          (let ((out (f)))
            (sys/dup2 original 1)
            (sys/close original)
            out))))))

(define run-synchronous-subprocess
  (if (function? run-synchronous-subprocess)
    run-synchronous-subprocess
    (lambda (exe args)
      (let ((pid (sys/fork)))
        (if (= 0 pid)
          (sys/execve exe (cons exe args) (list))
          (sys/wait pid))))))

(define (read-file p)
  (with-input-from-file p
    (lambda ()
      (let loop ((out (list)))
        (let ((sexpr (read)))
          (if (eof? sexpr)
            (reverse out)
            (loop (cons sexpr out))))))))

(define (read-std)
  (read-file "std.x"))

;;; READ

(define eof (lambda () #f)) ; sepxr will never be a function

(define _peek-c #f)

(define (peek-c)
  (if (equal? _peek-c #f)
    (set! _peek-c
      (let ((c (sys/read 0 1)))
        (if (equal? c "")
          eof
          c))))
  _peek-c)

(define (read-c)
  (if (eof? _peek-c) (error "reading after EOF"))
  (let ((out (peek-c)))
    (set! _peek-c #f)
    out))
  
(define (eof? c) (equal? c eof))

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

(define (read-quote)
  (read-c)
  (list 'quote (read)))

(define (read)
  (read-whitespace)
  (let ((c (peek-c)))
    (cond
      ((eof? c) c)
      ((equal? c "(")     (read-list))
      ((equal? c "#")     (read-boolean))
      ((string-number? c) (read-number))
      ((equal? c "\"")    (read-string))
      ((equal? c "'")     (read-quote))
      (#t                 (read-symbol)))))

;;; CPS

(define (cps-transform sexprs)
  (let loop ((sexprs sexprs) (out (reverse (defs sexprs))))
    (if (null? sexprs)
      (reverse out)
      (loop (cdr sexprs) (cons (list (list 'lambda '(_k) (cps (car sexprs) '_k)) '(lambda (v) (println v))) out)))))

(define (defs sexprs)
  (map (lambda (sexpr) (list 'define (cps-name (if (list? (second sexpr)) (caadr sexpr) (cadr sexpr))) #f))
       (filter (lambda (sexpr) (and (list? sexpr) (not (null? sexpr)) (equal? (car sexpr) 'define)))
               sexprs)))


(define (escape s)
  (string-append "\""
                 (cond ((equal? s "\\") "\\\\")
                       ((equal? s "\"") "\\\"")
                       ((equal? s "\n") "\\n")
                       (#t s))
                 "\""))

(define (cps sexpr kexpr)
  (cond
    ((boolean? sexpr) (list kexpr sexpr))
    ((number? sexpr)  (list kexpr sexpr))
    ((string? sexpr)  (list kexpr (escape sexpr)))
    ((symbol? sexpr)  (list kexpr (cps-name sexpr)))
    ((not (list? sexpr)) (error "cps: unknown sexpr: " sexpr))
    ((null? sexpr)    (list kexpr ''()))
    ((equal? (car sexpr) (symbol "define"))  (cps-define sexpr kexpr))
    ((equal? (car sexpr) (symbol "quote"))   (list kexpr sexpr))
    ((equal? (car sexpr) (symbol "set!"))    (cps-set! sexpr kexpr))
    ((equal? (car sexpr) (symbol "lambda"))  (cps-lambda sexpr kexpr))
    ((equal? (car sexpr) (symbol "let"))     (cps-let sexpr kexpr))
    ((equal? (car sexpr) (symbol "let*"))    (cps-let sexpr kexpr))
    ((equal? (car sexpr) (symbol "if"))      (cps-if sexpr kexpr))
    ((equal? (car sexpr) (symbol "cond"))    (cps-cond sexpr kexpr))
    ((equal? (car sexpr) (symbol "or"))      (cps-or sexpr kexpr))
    ((equal? (car sexpr) (symbol "and"))     (cps-and sexpr kexpr))
    ((equal? (car sexpr) (symbol "call/cc")) (cps-call/cc sexpr kexpr))
    ((equal? (car sexpr) (symbol "apply"))   (cps-apply sexpr kexpr))
    (#t                                      (cps-call sexpr kexpr))))

(define (cps-define sexpr kexpr)
  (if (list? (second sexpr))
    (cps-define-func sexpr kexpr)
    (cps-set! sexpr kexpr)))

(define (cps-set! sexpr kexpr)
  (define rv (gp))
  (define kv (gp))
  (list (list 'lambda (list kv) (cps (third sexpr) kv)) (list 'lambda (list rv) (list 'set! (cps-name (second sexpr)) rv) (list kexpr rv))))

(define (cps-define-func sexpr kexpr)
  (list 'let '()
    (list 'set! (cps-name (caadr sexpr)) (cps-func (func-named sexpr) (func-rest sexpr) (func-body sexpr)))
    (list kexpr (cps-name (caadr sexpr)))))

(define (split-args args)
  (let loop ((args args)
             (named '()))
    (if (null? args)
      (list (reverse named) #f)
      (if (equal? (car args) (symbol "."))
        (list (reverse named) (cadr args))
        (loop (cdr args) (cons (car args) named))))))

(define (func-named sexpr)
  (car (split-args (cdadr sexpr))))

(define (func-rest sexpr) 
  (cadr (split-args (cdadr sexpr))))

(define (func-body sexpr) (cddr sexpr))

(define (cps-lambda sexpr kexpr)
  (list kexpr (cps-func (lambda-named sexpr) (lambda-rest sexpr) (lambda-body sexpr))))

(define (lambda-named sexpr)
  (if (list? (second sexpr))
    (car (split-args (second sexpr)))
    (list)))

(define (lambda-rest sexpr)
  (if (list? (second sexpr))
    (cadr (split-args (second sexpr)))
    (second sexpr)))

(define (lambda-body sexpr) (cddr sexpr))

(define (cps-func named rest body)
  (define kv (gp))
  (append
    (list 'lambda
        (append
          (cons kv (map cps-name named))
          (if rest (list "." (cps-name rest)) (list))))
    (cps-body body kv)))

(define (cps-body sexprs kexpr)
  (if (null? sexprs)
    (error "empty body"))
  (define predefs (defs sexprs))
  (let loop ((sexprs (reverse sexprs))
             (next kexpr))
    (if (null? sexprs)
      (append predefs (list (list next #f)))
      (let ((rv (gp)))
        (loop (cdr sexprs)
            (list 'lambda '(_) 
              (cps (car sexprs) (list 'lambda (list rv) (list next rv)))))))))

(define (cps-let sexpr kexpr)
  (let* ((self (if (list? (second sexpr)) #f (cps-name (second sexpr))))
         (argps (map (lambda (p) (list (cps-name (car p)) (cadr p))) (if self (third sexpr) (second sexpr))))
         (kv (gp))
         (out (append
                (list (car sexpr))
                (if self (list self) '())
                (list (cons (list kv kexpr) (map (lambda (p) (list (car p) (car p))) argps)))
                (cps-body (if self (cdddr sexpr) (cddr sexpr)) kv))))
    (let loop ((argps (reverse argps)) (out out))
      (if (null? argps)
        out
        (loop (cdr argps)
              (cps (cadar argps) (list 'lambda (list (caar argps)) out)))))))

(define (cps-if sexpr kexpr)
  (define rv (gp))
  (cps (second sexpr) (list 'lambda (list rv) (list 'if rv (cps (third sexpr) kexpr) (if (> (length sexpr) 3) (cps (fourth sexpr) kexpr) #f)))))

(define (cps-cond sexpr kexpr)
  (let loop ((conds (reverse (cdr sexpr))) (out (list kexpr #f)))
    (define rv (gp))
    (if (null? conds)
      out
      (loop (cdr conds) (cps (caar conds) (list 'lambda (list rv) (list 'if rv (cps (cadar conds) kexpr) out)))))))

(define (cps-and sexpr kexpr)
  (let loop ((clauses (reverse (cdr sexpr)))
             (out (list kexpr #t)))
    (define rv (gp))
    (if (null? clauses)
      out
      (loop (cdr clauses) (cps (car clauses) (list 'lambda (list rv) (list 'if rv out (list kexpr #f))))))))

(define (cps-or sexpr kexpr)
  (let loop ((clauses (reverse (cdr sexpr)))
             (out (list kexpr #f)))
    (define rv (gp))
    (if (null? clauses)
      out
      (loop (cdr clauses) (cps (car clauses) (list 'lambda (list rv) (list 'if rv (list kexpr #t) out)))))))

(define (cps-apply sexpr kexpr)
  (define fv (gp))
  (define lv (gp))
  (cps (second sexpr)
       (list 'lambda (list fv)
             (cps (third sexpr)
                  (list 'lambda (list lv)
                        (list 'apply fv (list 'cons kexpr lv)))))))

(define gp
  (let ((i 0))
    (lambda ()
      (set! i (+ i 1))
      (symbol (string-append "v" (number->string i))))))

(define (cps-name s)
  (symbol (string-append (string s "/cps"))))

(define (cps-call sexpr kexpr)
  (define fs (cps-name (first sexpr)))
  (define pv (gp))
  (let loop ((args (reverse (cdr sexpr)))
             (out (list 'lambda (list pv) (list 'apply fs (list 'cons kexpr (list 'reverse pv))))))
    (if (null? args)
      (list out ''())
      (let ((inner-pv (gp))
            (rv (gp)))
        (loop (cdr args)
              (list 'lambda (list inner-pv) (cps (car args) (list 'lambda (list rv) (list out (list 'cons rv inner-pv))))))))))

(define (cps-call/cc sexpr kexpr)
  (error "call/cc not implemented"))
