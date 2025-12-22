; NOUNS

(define pass #t)
(define (test a b)
  (if (equal? a b)
    #t
    (let ()
      (set! pass #f)
      (println (list "FAIL:" a "!=" b))
      #f)))

; bools
(test #t #t)
(test #f #f)
(test #f (equal? #t #f))

; nums
(test 42 42)
(test 43 43)
(test #f (equal? 42 43))

; mixed
(test #f (equal? 0 #f))

; strings
(test "joe" "joe")
(test "henke" "henke")
(test "joe henke" (string-append "joe" " " "henke"))

; symbols
(test list list)
(test + +)
(let ((plus +))
  (test + plus))

; lists
(test (list 1 2 3) (append (list 1 2) (list 3)))

; VERBS
(test 3 (+ 1 2))
(test 5 (+ 2 3))
(test 15 (+ 4 5 6))
(test 23 (+ (+ 1 2) ( + 2 3) (+ 4 5 6)))

(test (list #t 42 "joe") (reverse (list "joe" 42 #t)))

(define a 1)
(test 1 a)
(define b (+ a 1))
(test 2 b)

(define c a)
(set! a 3)

(test 3 a)
(test 2 b)

; define funcs
(define (add-five x) (+ x (five))) ; notice forward ref
(define (five) 5)
(test 5 (five))
(test 10 (add-five (five)))
(test 15 (add-five 10))

; define lambda with closures
(define (adder x) (lambda (y) (+ x y)))
(define add-six (adder 6))
(define 18 (add-six 12))

; lets
(test 42 (let ((a 42))
           a))

(test 43 (let* ((a 42)
                (b (+ a 1)))
           b))

; if
(if #t (set! a "right") (set! a "wrong"))
(test "right" a)

(if #f (set! a "wrong2") (set! a "right2"))
(test "right2" a)

; cond
(define a 0)

(cond (#f (set! a 1))
      (#f (set! a 2))
      (#t (set! a 3)))

(test 3 a)

(test #f (cond (#f 1)))

; nested
(test 4
      (if #f
        (if #t 1 2)
        (if #f 3 4)))

(test 4 (cond (#f 1)
              (#f 2)
              (#t (cond (#f 3)
                        (#t 4)))))

; or
(define z 0)
(test #f (or #f #f))
(test #t (or #f (set! z 2)))
(test #t (or 0))
(test 2 (if 0 2 3))
(test 2 z)

(test #t (or #f #f #t))

; and
(define z #f)
(test #f (and #f (set! z #t)))
(test #f z)
(test #t (and #t (set! z #t)))
(test #t z)

; variadic func
(define (foo . args)
  args)
(test (list #t "two" 3) (foo #t "two" 3))

; variadic lambdas
(define bar (lambda (a . args) args))
(test (list "two" 3) (bar #t "two" 3))

(define baz (lambda args args))
(test (list #t "two" 3) (baz #t "two" 3))


; let loop
(test (list 0 1 2 3)
      (let loop ((x 0)
                 (out (list)))
        (if (< x 4)
          (loop (+ x 1) (cons x out))
          (reverse out))))

(define a (list 1 2 3))
(define b (list 4 5 6))
(test (list 1 2 3 4 5 6) (append a b))


(define (memo f)
  (let ((m (list)))
    (lambda args
      (let ((r (find (lambda (r) (equal? (car r) args)) m)))
        (if r
          (cadr r)
          (let ((r (apply f args)))
            (set! m (cons (list args r) m))
            r))))))

(define (fib n)
  (if (< n 3)
    1
    (apply + (map fib (map (curry - n) (list 1 2))))))

(set! fib (memo fib))

(test 2 (fib 3))
(test 3 (fib 4))
(test 5 (fib 5))
(test 8 (fib 6))
(test 12586269025 (fib 50))

;(let ((fd (open "/tmp/foo.txt" 1537 438)))
;  (write fd "bar" 3)
;  (close fd))
;
;(let ((fd (open "/tmp/foo.txt" 0)))
;  (println (read fd 3))
;  (close))
;
;(let ((pid (fork)))
;  (if (equal? pid 0)
;    (let ()
;      (println "c: listing cwd...")
;      (execve "/bin/ls" (list "/bin/ls") (list))
;      (println "c: error: execve failed"))
;    (let ()
;      (println "p: waiting...")
;      (wait pid)
;      (println "p: done")))
;  pid)

(if pass
  (println "PASS")
  (let ()
    (println "FAIL")
    (sys/exit 1)))
