(define lines
  (with-input-from-file "aoc/2025/day1.txt"
    (lambda ()
      (let loop ((current (list)) (out (list)))
        (let ((c (read-c)))
          (cond ((eof? c) (reverse out))
                ((equal? c "\n") (loop (list) (cons (apply string-append (reverse current)) out)))
                (#t (loop (cons c current) out))))))))

(define turns
  (map
    (lambda (s)
      (let ((l (string-list s)))
        (list
          (car l)
          (string->number (apply string-append (cdr l))))))
    lines))

(define (pmodulo x d)
  (modulo (+ (modulo x d) d) d))

(define (run)
  (let loop ((dial 50) (turns turns) (ended 0) (passed 0))
    (if (null? turns)
      (list ended passed)
      (let* ((turn (car turns))
             (d (car turn))
             (n (cadr turn))
             (m (if (equal? d "R") 1 (- 0 1)))
             (next (pmodulo (+ dial (* m n)) 100))
             (raw-next (+ dial (* m (modulo n 100)))))
        (loop
          next
          (cdr turns)
          (+ ended (if (= 0 next) 1 0))
          (+ (/ n 100)
             (if (or (and (> dial 0) (<= raw-next 0))
                     (>= raw-next 100))
               (+ passed 1)
               passed)))))))

(let* ((ans (run))
       (part-1 (car ans))
       (part-2 (cadr ans)))
  (print "Part 1: ")
  (println part-1)
  (print "Part 2: ")
  (println part-2))
