; breaks scheme, x, and xc
;(let loop ((x 1000000))
;  (if (< x 0) 0 (+ 1 (loop (- x 1)))))

; works in scheme, and xc
; takes forever in x with a lot of GC
; expected ??
;(let loop ((x 1000000) (out 0))
;  (if (< x 0)
;    0
;    (let ()
;      (let ()
;        (let ()
;          (let ()
;            (loop (- x 1) (+ out 1))))))))
