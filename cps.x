(for-each
  println
  (cps-transform (append (read-file "std.x") (read-file (last (command-line))))))
