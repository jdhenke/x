(define (string-split s sep)
  (let loop ((sl (string-list s))
             (cur (list))
             (out (list)))
    (if (null? sl)
      (reverse (cons (apply string-append (reverse cur)) out))
      (if (equal? (car sl) sep)
        (loop (cdr sl) (list) (cons (apply string-append (reverse cur)) out))
        (loop (cdr sl) (cons (car sl) cur) out)))))

(define (get-lines f)
  (with-input-from-file f
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

(define (distance p1 p2)
  (apply sum (map (lambda (xs) (square (- (car xs) (cadr xs)))) (zip p1 p2))))

(define (pairs l)
  (let loop ((l1 l)
             (l2 (cdr l))
             (out '()))
    (cond ((null? (cdr l1)) out)
          ((null? l2) (loop (cdr l1) (cddr l1) out))
          (#t (loop l1 (cdr l2) (cons (list (car l1) (car l2)) out))))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (#t (member x (cdr l)))))

(define (overlap? a b)
  (find (lambda (x) (member x b)) a))

(define (connected-components nodes edges)
  (let loop ((edges edges)
             (comps (map (lambda (n) (list n)) nodes)))
    (println (length edges))
    (map println (sort comps (lambda (c1 c2) (< (length c2) (length c1)))))
    (if (null? edges)
      comps
      (let inner ((comps comps)
                  (conns '())
                  (rest '()))
        (cond ((null? comps) (loop (cdr edges) (cons (apply append conns) rest)))
              ((overlap? (car comps) (car edges)) (inner (cdr comps) (cons (car comps) conns) rest))
              (#t (inner (cdr comps) conns (cons (car comps) rest))))))))

;;; DAY-8

(define (day-8 f n)
  (let* ((points
           (map
             (lambda (l)
               (map string->number (string-split l ",")))
             (get-lines f)))
         (closest
           (map
             cadr
             (sublist
               (sort
                 (map (lambda (ps) (list (apply distance ps)
                                         ps))
                      (pairs points))
                 (lambda (i1 i2) (< (car i1) (car i2))))
               0
               n))))
    (apply product
      (sublist
        (sort (map length (connected-components points closest)) >)
        0
        3))))

;(println (distance '(162 817 812) '(57 618 57)))
(println "Day 8 Part 1 Test:" (day-8 "./aoc/2025/day8-test.txt" 10))
(println "Day 8 Part 1" (day-8 "./aoc/2025/day8.txt" 100))
