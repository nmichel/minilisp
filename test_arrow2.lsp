((defmacro insert-at-end (x form)
   (if (list? form)
       (quasiquote ((unquotesplicing form) (unquote x)))
       (quasiquote ((form) (unquote (list x))))
       )
   
   (print (insert-at-end (+ 4 12) (+ 1 2 3)))
   (print (insert-at-end 2 (+ 1 2 3)))
   ;; (insert-at-end 2 print)

   ((defmacro ->> (form)
      (if (empty? (cdr (cdr form)))
          (quasiquote (insert-at-end (unquote (car form)) (unquote (car (cdr form)))))
          (quasiquote
           (->> ((insert-at-end (unquote (car form)) (unquote (car (cdr form))))
                 (unquotesplicing (cdr (cdr form)))))
           )
          )
      
      (print "->> pass : "
             (if
                 (== (list 4 3 1 2)
                     (->> ((list 1 2) (cons 3) (cons 4))))
                 true
                 false))
      ))
   ))
