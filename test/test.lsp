;; Each expression is evaluated in the context of the previous one
;; 
(defmacro seq (body)
  (if (empty? (cdr body))
      (quasiquote ~(car body)) ;; can't use ` here because the parser is flawed
      (if (or (== (car (car body)) 'define)
              (== (car (car body)) 'defmacro))
          `((~@(car body) (seq ~(cdr body))))
          `((lambda () ~(car body) (seq ~(cdr body))))))

  (seq ((define (step (lambda (n l)
                        ((define (step-n (lambda (f curr n l)
                                           (if (empty? l)
                                               l
                                               (if (== curr n)
                                                   (cons (car l) (f f 1 n (cdr l)))
                                                 (f f (+ curr 1) n (cdr l))))))
                           (step-n step-n 1 n l))
                        )))
          )

        (print "step: " (step 2 '(1 2 3 4)))
        
        (defmacro cond (body)
          ;; Expand a list of (cond, code) pair into a structure of nested "if/else" forms.
   
          (if (empty? body)
              (quasiquote true)
              (quasiquote
               (if (unquote (car (car body)))
                   (unquote (car (cdr (car body))))
                   (cond (unquote (cdr body)))))))

        (define (*2 (lambda (x) (+ x x))))
        (define (*4 (lambda (x) (*2 (*2 x)))))
        (print "coucou" (*2 2) (*4 2))
        (print "toto")
        (print "cond ?!"
               (cond (((== 2 1) "prout")
                      ((== 1 1) (+ 1 2))
                      (true "toutou"))))

        ;; Test recursive fonction, passing itself as call parameter
        ;; 
        (define (foo (lambda (f x)
                       (if (empty? x)
                           x
                           (f f (cdr x)))
                       )))

        (foo foo (list 1 2 3))
        
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
              ))

        (defmacro test (name test expected)
          (quasiquote
           ((lambda () ((lambda (r) (print "test " (unquote name) " : " r))
                   (if (== (unquote test) (unquote expected))
                       "pass"
                       "failed"))
              ))))
      
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
  )
