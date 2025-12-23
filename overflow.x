; tail call optimization
;(define (bad-sum x)
;  (if (< x 1)
;    0
;    (+ 1 (bad-sum x))))
;(bad-sum 100000)

(let loop ((x 100000) (out 0))
  (if (= (modulo x 1000) 0) (println x))
  (if (< x 0) out (loop (- x 1) (+ out 1))))
