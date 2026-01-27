(define (foo)
  (let ((i 0))
    (defer (println "top"))
    (defer (println i))
    (defer (println "bottom"))
    (set! i 1)))

(foo)

(define (bar)
  (let ((i 0))
    (defer (defer (println "top")))
    (defer (defer (println i)))
    (defer (defer (println "bottom")))
    (set! i 1)))

(bar)

