(defmacro -> (body)
  (if (empty? (cdr (cdr body)))
      (quasiquote ((unquote (car (car (cdr body))))
                   (unquote (car body))
                   (unquotesplicing (cdr (car (cdr body))))))
    (quasiquote
     (-> (((unquote (car (car (cdr body))))
           (unquote (car body))
           (unquotesplicing (cdr (car (cdr body))))) (unquotesplicing (cdr (cdr body))))
         )
     )
    )
   
  (defmacro test (name test expected)
    (quasiquote
     ((lambda () ((lambda (r) (print "test " (unquote name) " : " r))
             (if (== (unquote test) (unquote expected))
                 "pass"
                 "failed"))
        )))
      
    (test "vaut 3" (+ 1 2) 3)
    (test "vaut encore 3" (+ 1 2) ((lambda (r) r) 3))
    (test "vaut toujours 3"
          ((lambda (r) r) 3)
          (-> ((+ 1 3) (- 10) (+ 9))))

    (test "vaut 9"
          (+  1 2 (- 2 3) ((lambda (a b) (+ a b))
                           3 4))
          9)

    (test "define vaut 45"
          ((define (foo 42
                    bar (lambda (x y) (+ x y)))
             (+ foo (bar 1 2))
             ))
          45)

    (test "define in define vaut " 
          ((define (foo 42
                    bar (lambda (a b foo) (+ a b foo)))
             ((define (foo 1
                       neh (lambda (a f) (f a a foo)))
                (neh foo bar)))))
          44)
    )
  )
