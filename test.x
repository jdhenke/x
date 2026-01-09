(define pass #t)
(define (test a b)
  (if (equal? a b)
    #t
    (let ()
      (set! pass #f)
      (println (list "FAIL:" a "!=" b))
      (sys/exit 1)
      #f)))

; NOUNS

; bools
(test #t #t)
(test #t (equal? #f #f))
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
(test 18 (add-six 12))

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

; (test #f (cond (#f 1))) ; FIXME: diff in MIT scheme

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
;(test #t (or #f (set! z 2)))
;(test #t (or 0))
(test 2 (if 0 2 3))
;(test 2 z)

(test #t (or #f #f #t))

; and
(define z #f)
(test #f (and #f (set! z #t)))
(test #f z)
;(test #t (and #t (set! z #t)))
;(test #t z)

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
    (let ()
      (apply + (map fib (map (curry - n) (list 1 2))))))) ; purposefully crazy

(set! fib (memo fib))

(test 1 (fib 1))
(test 1 (fib 2))
(test 2 (fib 3))
(test 3 (fib 4))
(test 5 (fib 5))
(test 8 (fib 6))

(test "420" (number->string 420))
(test "0" (number->string 0))
(test 420 (string->number "420"))
(test 0 (string->number "0"))
(test 9 (string->number "9"))
(test 10 (string->number "10"))
(test 11 (string->number "11"))
(test (string->number "9") 9)
(test (string->number "10") 10)
(test (string->number "11") 11)

(test #f (string-number? "a"))
(test #f (string-number? ""))
(test #t (string-number? "1"))
(test #t (string-number? "0"))

(if (not pass)
  (let ()
    (println "FAIL")
    (sys/exit 1)))

