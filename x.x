;;; syscalls

;;; core interpreter

(define (read)
  (read-whitespace)
  (let ((c (peek-char)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\#) (read-chr))
      ((char=? c #\') (read-literal))
      ((char=? c #\() (read-list))
      ((char=? c #\") (read-str))
      ((char-numeric? c) (read-number))
      (else (read-symbol)))))

(define (log x s)
  (display s)
  (display " ")
  (display x)
  (newline)
  x)

(define (read-chr)
  (let* ((a (read-char))
         (b (read-char))
         (s (list a b)))
    (if (char=? (cadr s) #\\)
      (set! s (append s (list (read-char)))))
    (list 'char (list->string (append s (read-char-matching (lambda (c) 
          (not
            (or
              (eof-object? c)
              (char=? c #\))
              (char-whitespace? c)
              (char=? c #\newline))))))))))

(define (read-literal)
  (read-char)
  (list 'literal (read)))

(define (read-list)
  (read-char)
  (read-whitespace)
  (let loop ((vals '()))
    (if (char=? (peek-char) #\))
      (begin
        (read-char)
        (cons 'list vals))
      (let ((val (read)))
        (if (eof-object? val)
          (error "unexpected EOF reading list")
          (loop (append vals (list val))))))))

(define (read-whitespace)
  (read-char-matching (lambda (c) (or (char-whitespace? c) (char=? c #\newline))))
  (let ((c (peek-char)))
    (if (eof-object? c)
      c
      (if (char=? (peek-char) #\;)
        (begin
          (read-char-matching (lambda (c) (not (char=? c #\newline))))
          (read-whitespace))
        #f))))

(define (read-number)
  (list 'number (string->number (list->string (read-char-matching char-numeric?)))))

(define (read-char-matching f)
  (let ((c (peek-char)))
    (cond ((or (eof-object? c) (not (f c))) (list))
          (else (begin
                  (read-char) 
                  (cons c (read-char-matching f)))))))

(define (read-symbol)
  (list
    'symbol
    (list->string
      (read-char-matching
        (lambda (c) 
          (not
            (or
              (eof-object? c)
              (char=? c #\))
              (char-whitespace? c)
              (char=? c #\newline))))))))

(define (read-str)
  (read-char)
  (let ((s (read-char-matching (lambda (c) (not (char=? c #\"))))))
    (if (char=? (peek-char) #\")
      (begin
        (read-char)
        (list 'string (list->string s)))
      (error "invalid end of string" (peek-char)))))

(let loop ()
  (let ((sexpr (read)))
    (if (eof-object? sexpr)
      #t
      (begin 
        (pretty-print sexpr)
        (newline)
        (loop)))))

;(define (eval sexpr))
;
;(define (print sexpr))
;
;;;; standard library
;(define (repl)
;  (let loop ()
;    (print (eval (read)))
;    (loop)))
;
;;;; main
;(repl)
