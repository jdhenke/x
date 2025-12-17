;; NOUNS
;
;; bools
;#t
;#f
;
;; nums
;42
;43
;
;; strings
;"joe"
;"henke"
;
;; symbols
;list
;list
;+
;+
;
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
;
;(define (add-five x) (+ x (five))) ; notice forward ref
;(define (five) 5)
;(five)
;(add-five (five))
;(add-five 10)
;
;(define (adder x) (lambda (y) (+ x y)))
;(define add-six (adder 6))
;(add-six 12)
;
;(let ((a 42))
;  a)
;
;a
;
;(let* ((a 42)
;      (b (+ a 1)))
;  b)
;
;(if #t (set! a "right") (set! a "wrong"))
;a
;(if #f (set! a "wrong") (set! a "right"))
;a
;
;(define a 0)
;
;(cond (#f (set! a 1))
;      (#f (set! a 2))
;      (#t (set! a 3)))
;
;a
;
;(cond (#f 1))
;
;(define z 0)
;(or #f #f)
;(or #f (set! z 2))
;z
;(or #f #f #t)
;
;(define z #f)
;(and #f (set! z #t))
;z
;(and #t (set! z #t))
;z

(define (foo . args)
  args)

(foo #t "two" 3)
