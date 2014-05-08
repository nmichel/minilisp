(+  1 2 (- 2 3) ((lambda (a b)
		   (+ a b))
		 3 4))

((define (foo 42
	  bar (lambda (x y) (+ x y)))
   (+ foo (bar 1 2))
   ))

((define (foo 42
	  bar (lambda (a b foo) (+ a b foo)))
   ((define (foo 1
	     neh (lambda (a f) (f a a foo)))
      (neh foo bar)))))
