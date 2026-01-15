(let loop ()
  (let ((s (read)))
    (if (eof-object? s)
      'done
      (let ()
        (pp s)
        (loop)))))
