(for-each (lambda (l) (println l)) (cps-transform (read-file "test.x")))
