(define (string-split s sep)
  (let loop ((sl (string-list s))
             (cur (list))
             (out (list)))
    (if (null? sl)
      (reverse (cons (apply string-append (reverse cur)) out))
      (if (equal? (car sl) sep)
        (loop (cdr sl) (list) (cons (apply string-append (reverse cur)) out))
        (loop (cdr sl) (cons (car sl) cur) out)))))

(define (get-lines p)
  (with-input-from-file p
    (lambda ()
      (let loop ((current (list)) (out (list)))
        (let ((c (read-c)))
          (cond ((eof? c) (reverse out))
                ((equal? c "\n") (loop (list) (cons (apply string-append (reverse current)) out)))
                (#t (loop (cons c current) out))))))))


(define (sum . args)
  (let loop ((args args)
             (out 0))
    (if (null? args)
      out
      (loop (cdr args) (+ (car args) out)))))

(define (product . args)
  (let loop ((args args)
             (out 1))
    (if (null? args)
      out
      (loop (cdr args) (* (car args) out)))))

(define (square x)
  (* x x))

;(define (distance p1 p2)
;  (apply sum (map (lambda (xs) (square (- (car xs) (cadr xs)))) (zip p1 p2))))

(define (distance p1 p2)
  (let ((dx (- (car p1) (car p2)))
        (dy (- (cadr p1) (cadr p2)))
        (dz (- (caddr p1) (caddr p2))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

(define (map-pairs f l)
  (let loop ((l1 l)
             (l2 (cdr l))
             (out '())
             (n 0))
    (if (= 0 (modulo n 1000)) (println n))
    (cond ((null? (cdr l1)) out)
          ((null? l2) (loop (cdr l1) (cddr l1) out n))
          (#t (loop l1 (cdr l2) (cons (f (car l1) (car l2)) out) (+ n 1))))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (#t (member x (cdr l)))))

(define (overlap? a b)
  (find (lambda (x) (member x b)) a))

(define (connected-components nodes edges f)
  (let loop ((edges edges)
             (comps (map (lambda (n) (list n)) nodes))
             (last #f)
             (n 0))
    (let ((out (f n last comps)))
      (if out
        out
        (let inner ((comps comps)
                    (conns '())
                    (rest '()))
          (cond ((null? comps) (loop (cdr edges) (cons (apply append conns) rest) (car edges) (+ n 1)))
                ((overlap? (car comps) (car edges)) (inner (cdr comps) (cons (car comps) conns) rest))
                (#t (inner (cdr comps) conns (cons (car comps) rest)))))))))

;;; DAY-8

(define (day-8 p f)
  (let* ((points
           (map
             (lambda (l)
               (map string->number (string-split l ",")))
             (get-lines p)))
         (tmp (println "pairing..."))
         (pairs (map-pairs (lambda (x1 x2)
                              (list (distance x1 x2)
                                    (list x1 x2)))
                            points))
         (tmp (println "sorting..."))
         (edges (map cadr (sort
                 pairs
                 (lambda (i1 i2) (< (car i1) (car i2)))))))
    (println "connecting...")
    (connected-components points edges f)))

(define (part-1-stop s)
  (lambda (n e comps)
    (if (< n s)
      #f
      (apply product
        (sublist
          (sort (map length comps) >)
          0
          3)))))

(define (part-2-stop)
  (lambda (n e comps)
    (if (null? (cdr comps))
      (* (caar e) (caadr e))
      #f)))

(println "Day 8 Part 1 Test:" (day-8 "./aoc/2025/day8-test.txt" (part-1-stop 10)))
;(println "Day 8 Part 1" (day-8 "./aoc/2025/day8.txt" (part-1-stop 1000)))
(println "Day 8 Part 2 Test:" (day-8 "./aoc/2025/day8-test.txt" (part-2-stop)))
(println "Day 8 Part 2" (day-8 "./aoc/2025/day8.txt" (part-2-stop)))
