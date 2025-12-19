(let ((fd (open "/tmp/foo.txt" 1537 438)))
  (write fd "bar\n" 4)
  (close fd))

(let ((fd (open "/tmp/foo.txt" 0)))
  (println (read fd 4))
  (close))
