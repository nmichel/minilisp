((defmacro -> (body)
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
   
   (-> (4 (+ 1) (- 2) (+ 4 5) (print " !")))
   ))

