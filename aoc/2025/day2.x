(define (string-split s sep)
  (let loop ((sl (string-list s))
             (cur (list))
             (out (list)))
    (if (null? sl)
      (reverse (cons (apply string-append (reverse cur)) out))
      (if (equal? (car sl) sep)
        (loop (cdr sl) (list) (cons (apply string-append (reverse cur)) out))
        (loop (cdr sl) (cons (car sl) cur) out)))))

(define (get-ranges)
  (map
    (lambda (s) (map string->number (string-split s "-")))
    (string-split
      (with-input-from-file "aoc/2025/day2.txt"
        (lambda ()
          (let loop ((out '()))
            (let ((c (read-c)))
              (if (equal? "\n" c) ; all on one line in this case
                (apply string-append (reverse out))
                (loop (cons c out)))))))
      ",")))

(define (p1 n)
  (let* ((s (number->string n))
         (n (string-length s))
         (m (/ n 2)))
    (equal? (substring s 0 m) (substring s m n))))

(define (p2 n) #f)

(let ((rs (get-ranges)))
  (let outer ((rs rs) (part1 0) (part2 0))
    (if (null? rs)
      (let ()
        (println (string-append "Part 1: " (number->string part1)))
        (println (string-append "Part 2: " (number->string part2))))
      (let* ((rg (car rs))
             (l (car rg))
             (r (cadr rg)))
        (println rg)
        (let inner ((x l) (part1 part1) (part2 part2))
          (if (> x r)
            (outer (cdr rs) part1 part2)
            (inner (+ x 1) (+ part1 (if (p1 x) x 0)) (+ part2 (if (p2 x) x 0)))))))))
  
