((defmacro cond (body)
   ;; Expand a list of (cond, code) pair into a structure of nested "if/else" forms.
   
   (if (empty? body)
       `true
       `(if ~(car (car body))
	    ~(car (cdr (car body)))
	    (cond ~(cdr body))))
   
   (cond (((== 1 2) (list 1 2 3))
	  ((== 3 1) (list 4 5 6))
	  ((== 1 0) (list 7 8 9))
	  (true (list "a" "b" "c"))))

   ((defmacro insert-at-end (x form)
      (if (list? form)
          `(~@form ~x)
          `((form) ~(list x))
          )
   
      (print (insert-at-end (+ 4 12) (+ 1 2 3)))
      (print (insert-at-end 2 (+ 1 2 3)))
      ;; (insert-at-end 2 print)

      ((defmacro ->> (form)
         (if (empty? (cdr (cdr form)))
             `(insert-at-end ~(car form) ~(car (cdr form)))
             `(->> ((insert-at-end ~(car form) ~(car (cdr form)))
                    ~@(cdr (cdr form))))
             )
      
         (print "->> pass : "
                (if
                    (== (list 4 3 1 2)
                        (->> ((list 1 2) (cons 3) (cons 4))))
                    true
                    false))
         ))
      ))

   (print "sugar quote: " '(+ 42 0))
   (print "sugar quote quote: " ''(+ 42 0))
   (print "sugar quote unquote: " '~(+ 42 0))
   (print "sugar quote unquotesplicing: " '~@(+ 42 0))
   (print "sugar quote quasiquote: " '`(+ 42 0))
   ))

