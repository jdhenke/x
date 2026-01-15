(define + (let ((orig +)) (lambda (k . args) (k (apply orig args)))))
