;;; AMB

(define amb-fail (lambda () (error "amb fail")))

(define (amb . opts)
  (if (null? opts)
    (amb-fail)
    (let ((prev-fail amb-fail))
      (call/cc
        (lambda (sk)
          (call/cc
            (lambda (fk)
              (set! amb-fail
                (lambda ()
                  (set! amb-fail prev-fail)
                  (fk 'fail)))
              (sk (car opts))))
          (set! amb-fail prev-fail)
          (apply amb (cdr opts)))))))

(define (ramb . opts)
  (apply amb (shuffle opts)))

(define (require p)
  (println "before (amb)")
  (if (not p) (amb)))

(define (rambzip l r f)
  (define (choose n opts)
    (let ((choice (apply ramb (filter (f n) opts))))
      (let loop ((h opts))
        (if (equal? (car h) choice)
          (let ()
            (set-car! h (car opts))
            (set-car! opts choice)
            choice)
          (loop (cdr h))))))
  (let loop ((h l)
             (r (copy r))
             (out '()))
    (let ((h (sort-by h < (lambda (n) (length (filter (f n) r))))))
      (if (null? h)
        (sort-by
          (sort-by out string<? cadr)
          string<?
          car)
        (let ((choice (choose (car h) r)))
          (loop (cdr h) (cdr r) (cons (list (car h) choice) out)))))))

;;; GENERIC

(define (curry f . largs)
  (lambda rargs (apply f (append largs rargs))))

(define (compose f g) (lambda (x) (f (g x))))

(define (copy l) (map (lambda (x) x) l))

(define (many x n)
  (let loop ((i 0)
             (out '()))
    (if (< i n)
      (loop (+ i 1) (cons x out))
      (reverse out))))

(define (mapply f l)
  (map (curry apply f) l))

(define (flatten l) (apply append l))

(define (none-of . args)
  (lambda (x) (not (find (curry equal? x) args))))

(define (shuffle lst)
  (let loop ((remaining lst)
             (acc '()))
    (if (null? remaining)
        acc
        (let ((n (random (length remaining) (make-random-state #t))))
          (let ((elem (list-ref remaining n))
                (rest (remove-nth remaining n)))
            (loop rest (cons elem acc)))))))

(define (remove-nth lst n)
  (let loop ((lst lst)
             (n n)
             (acc '()))
    (cond ((null? lst) (reverse acc))
          ((= n 0) (append (reverse acc) (cdr lst)))
          (#t (loop (cdr lst) (- n 1) (cons (car lst) acc))))))

(define (sort-by l cmp get)
  (sort l (lambda (a b) (cmp (get a) (get b)))))

;;; ELF

(define (solve counts)
  (let ((all (flatten (mapply many counts))))
    (rambzip all all (lambda (n) (none-of n (partner n))))))

(define (partner x)
  (let ((pairs '(("mom" "dad")
                 ("joe" "mp")
                 ("sam" "ty")
                 ("sara" "ab"))))
    (cadr (assoc x (append pairs (map reverse pairs))))))

(define counts
    '(("mom"  1)
      ("dad"  1)
      ("joe"  1)
      ("sam"  1)
      ("sara" 1)
      ("mp"   1)
      ("ty"   1)
      ("ab"   1)))

;;; MAIN

(for-each
  (lambda (s)
    (print (car s))
    (print " ==> ")
    (print (cadr s))
    (print "\n"))
  (solve counts))

