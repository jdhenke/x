(for-each (lambda (l) (println l)) (cps-transform (append (read-file "/tmp/amb.scm"))))
