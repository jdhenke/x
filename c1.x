; NOUNS

; bools
#t
#f

; nums
42
43

; strings
"joe"
"henke"

; symbols
list
list
+
+

; VERBS
(+ 1 2)
(+ 2 3)
(+ 4 5 6)
(+ (+ 1 2) ( + 2 3) (+ 4 5 6))

(list #t 42 "joe")
(list #t 42 "joe" (list "a" "b" "c"))

;(define a 1)
;(define b a)
;(set! a 2)
;a ; want 2
;b ; want 1


; (define (five) 5)
; (define (add-five x) (+ x 5))
; (define (adder x) (lambda (y) (+ x y)))
