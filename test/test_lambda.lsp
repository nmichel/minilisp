((define
   (f (lambda (t) (if t + -)))
   ((define (y (f true)
             n (f false))
      (print "yes " (y 1 2))
      (print "no " (n 1 2))))))


; Recursive function does not work, obviously, as the folowing illustrates
; 
; ((define (foo (lambda (l) (if (empty? l) true (foo (cdr l)))))
;   (foo (list 1 2 3))))
