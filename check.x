(let ((fd (open "/tmp/foo.txt" 1537 438)))
  (write fd "bar" 3)
  (close fd))

(let ((fd (open "/tmp/foo.txt" 0)))
  (println (read fd 3))
  (close))

(let ((pid (fork)))
  (if (equal? pid 0)
    (let ()
      (println "c: echoing")
      (execve "/bin/echo" (list "/bin/echo" "c: echo: foo") (list))
      (println "c: error: execve failed"))
    (let ()
      (println "p: waiting...")
      (wait pid)
      (println "p: done")))
  pid)

(exit 42)

