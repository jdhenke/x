;; TODO: write program from which this framework emerges
;; TODO: write a program that proves any combination

;; Check binary equality (black box)
; C = Scc = Sicc = Icc = Iicc = Cc
; I = Sci = Sici = Ici = Iici = Ci
; T = Sct = Sict = Ict = Iict = Ct

;; Check output equality (glass box)
; T = St  = Sit  = It  = Iit

; Verify all possible combinations

(define (next l)
  (cond
    ((equal? (car l) "s") (list "i" "c" "t"))
    ((equal? (car l) "i") (list "i" "c" "t"))
    ((equal? (car l) "c") (list "i" "c" "t"))
    ((equal? (car l) "t") (list))))

(define (out? l)
  (cond ((equal? (car l) "s") #f)
        ((equal? (car l) "t") #t)
        ((pair? l)
         (cond ((equal? (cadr l) "c") #t)
               (#t #f)))))

(define (run? l)
  (cond ((equal? (car l) "s") #t)
        ((equal? (car l) "i") #t)
        ((equal? (car l) "c") #t)
        (#t #f)))

(define (cull? l)
  (or
    (> (length l) 6)))

(define (nout? l) (not (out? l)))

(let loop ((live (list (list "s"))) (out (list)))
  (if (null? live)
    (map println (map (lambda (l) (apply string-append l)) (map reverse out)))
    (let ((l (car live)))
      (if (cull? l)
        (loop (cdr live) out)
        (loop
          (append
            (cdr live)
            (map (lambda (n) (cons n l)) (next l)))
          (if (out? l) (cons l out) out))))))
