((lambda (s)
   ((eval s (interaction-environment)) s))
 '(lambda
   (s)
   (pretty-print
    (list
     '(lambda (s) ((eval s (interaction-environment)) s))
     (list 'quote s)))))