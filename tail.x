(define (count-to-infinity n)
  (if (equal? n 0)
      "done"
      (count-to-infinity (- n 1))))
(count-to-infinity 100000)

