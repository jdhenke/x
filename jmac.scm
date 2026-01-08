(define jmac (car (read-file "jmac.x")))

(define env (list
              (list '#t #t)
              (list '#f #f)
              (list 'list? list?)
              (list 'equal? equal?)
              (list 'cons cons)
              (list 'car car)
              (list 'cdr cdr)))

(display "JMAC 1")
(newline)
(define jmac1 (eval jmac (interaction-environment)))
(pretty-print (jmac1 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

(display "JMAC 2")
(newline)
(define jmac2 (jmac1 jmac env))
(pretty-print (jmac2 'b '((b 52))))
(newline)
(pretty-print (jmac2 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

(display "JMAC 3")
(newline)
(define jmac3 (jmac2 jmac env))
(pretty-print (jmac3 '((lambda (a c) (cons a (cons c (quote ())))) b b) (append '((b 52)) env)))
(newline)

;(display "JMAC 4")
;(newline)
;(define jmac4 (jmac3 jmac env))
;(pretty-print (jmac4 '((lambda (a c) (cons a (cons c (quote ())))) b b) '((b 52))))
;(newline)

