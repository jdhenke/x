(define (get-lines)
  (with-input-from-file "aoc/2025/day3.txt"
    (lambda ()
      (let loop ((current (list)) (out (list)))
        (let ((c (read-c)))
          (cond ((eof? c) (reverse out))
                ((equal? c "\n") (loop (list) (cons (apply string-append (reverse current)) out)))
                (#t (loop (cons c current) out))))))))

(define (max . l)
  (let loop ((m #f)
             (l l))
    (if (null? l)
      m
      (loop (if (or (not m) (> (car l) m)) (car l) m) (cdr l)))))

(define (max-with-index l)
  (let ((m (apply max l)))
    (let loop ((i 0))
      (if (>= i (length l))
        #f
        (if (= m (list-ref l i))
          (list m i)
          (loop (+ i 1)))))))

(define (string->number-list s)
  (let loop ((i 0) (out '()))
      (if (>= i (string-length s))
        (reverse out)
        (loop (+ i 1) (cons (string->number (substring s i (+ i 1))) out)))))

(define (expt b e)
  (if (= e 0) 1 (* b (expt b (- e 1)))))

(define (sum . args)
  (let loop ((args args)
             (out 0))
    (if (null? args)
      out
      (loop (cdr args) (+ (car args) out)))))

(define (day-3 num-batteries)  
  (define (max-joltage remaining bank)
    (if (< remaining 0)
      0
      (let* ((l (max-with-index (sublist bank 0 (- (length bank) remaining))))
             (max-val (car l))
             (max-idx (cadr l)))
        (+
          (* (expt 10 remaining) max-val)
          (max-joltage (- remaining 1) (sublist bank (+ max-idx 1) (length bank)))))))
  (define parse-bank string->number-list)
  (apply sum (map (curry max-joltage (- num-batteries 1)) (map parse-bank (get-lines)))))

(println "Day 3 Part 1:" (day-3 2))  ;; 16842
(println "Day 3 Part 2:" (day-3 12)) ;; 167523425665348

