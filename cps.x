(for-each (lambda (l) (println l)) (cps-transform (append (read-file "std.x") (read-file "/tmp/elf.scm"))))
