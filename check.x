; NOUNS

; FIXME: better test utilites
(define (test a b)
  (if (equal? a b)
    #t
    (let ()
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

;; symbols
(test list list)
(test + +)
(let ((plus +))
  (test + plus))

; lists
(test (list 1 2 3) (append (list 1 2) (list 3)))

(println (append (list 1) (list 2 3) (list 4 5 6)))

;; VERBS
;(+ 1 2)
;(+ 2 3)
;(+ 4 5 6)
;(+ (+ 1 2) ( + 2 3) (+ 4 5 6))
;
;(list #t 42 "joe")
;(list #t 42 "joe" (list "a" "b" "c"))
;
;(define a 1)
;a
;(define b (+ a 1))
;b
;
;(define c a)
;(set! a 3)
;a
;c
;;a ; want 3
;;b ; want 1
;
;; define funcs
;(define (add-five x) (+ x (five))) ; notice forward ref
;(define (five) 5)
;(five)
;(add-five (five))
;(add-five 10)
;
;; define lambda with closures
;(define (adder x) (lambda (y) (+ x y)))
;(define add-six (adder 6))
;(add-six 12)
;
;; lets
;(let ((a 42))
;  a)
;a
;
;(let* ((a 42)
;      (b (+ a 1)))
;  b)
;
;; if
;(if #t (set! a "right") (set! a "wrong"))
;a
;(if #f (set! a "wrong") (set! a "right"))
;a
;
;; cond
;(define a 0)
;
;(cond (#f (set! a 1))
;      (#f (set! a 2))
;      (#t (set! a 3)))
;a
;
;(cond (#f 1))
;
;; or
;(define z 0)
;(or #f #f)
;(or #f (set! z 2))
;z
;(or #f #f #t)
;
;; and
;(define z #f)
;(and #f (set! z #t))
;z
;(and #t (set! z #t))
;z
;
;; variadic func
;(define (foo . args)
;  args)
;(foo #t "two" 3)
;
;(define (foo2 a . args)
;  args)
;(foo2 #t "two" 3)
;
;; variadic lambdas
;(define bar (lambda (a . args) args))
;(bar #t "two" 3)
;
;(define baz (lambda args args))
;(baz #t "two" 3)
;
;
;; let loop
;(let loop ((x 0))
;  (if (< x 4)
;    (let ()
;      (println x)
;      (loop (+ x 1)))))
;
;(define a (list 1 2 3))
;(define b (list 4 5 6))
;(define c (append a b))
;a
;b
;c
;
;
;(map (curry + 3) (list 1 2))
;
;; std
;    
;(define (memo f)
;  (let ((m (list)))
;    (lambda args
;      (let ((r (find (lambda (r) (equal? (car r) args)) m)))
;        (if r
;          (cadr r)
;          (let ((r (apply f args)))
;            (set! m (cons (list args r) m))
;            r))))))
;
;
;(define (fib n)
;  (if (< n 3)
;    1
;    (apply + (map fib (map (curry - n) (list 1 2))))))
;
;(set! fib (memo fib))
;
;
;(let loop ((i 1))
;  (if (< i 11)
;    (let ()
;      (println (list "fib" i))
;      (println (fib i))
;      (loop (+ i 1)))
;     #f))
;
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
;
;(exit 42)
;
