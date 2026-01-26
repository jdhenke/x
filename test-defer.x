(define (foo)
  (let ((i 0))
    (defer (println "top"))
    (defer (println i))
    (defer (println "bottom"))
    (set! i 1)))

(foo)
