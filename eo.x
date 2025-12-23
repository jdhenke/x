(define (even x)
  (if (= x 0)
    #t
    (odd (- x 1))))

(define (odd x)
  (if (= x 1)
    #t
    (even (- x 1))))

(println (even 10000000))

(define (evenc x)
  (cond ((= x 0) #t)
         (#t (oddc (- x 1)))))

(define (oddc x)
  (cond ((= x 1) #t)
         (#t (evenc (- x 1)))))

(println (evenc 10))
(println (evenc 10000000))

