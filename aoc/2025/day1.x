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

(define (part-1)
  (let loop ((dial 50) (turns turns) (out 0))
    (if (null? turns)
      out
      (let* ((turn (car turns))
             (d (car turn))
             (n (cadr turn))
             (m (if (equal? d "R") 1 (- 0 1)))
             (next (modulo (+ 100 (+ dial (* m n))) 100)))
        (loop next (cdr turns) (+ out (if (= 0 next) 1 0)))))))

(print "Part 1: ")
(println (part-1))
