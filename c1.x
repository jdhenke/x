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
(define a 1)
a
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

(let ((a 42))
  a)

a

(let ((a 42)
      (b (+ a 1)))
  b)
