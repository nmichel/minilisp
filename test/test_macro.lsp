(defmacro cond (body)
  (if (empty? body)
      (quasiquote true)
      (quasiquote
       (if (unquote (car (car body)))
	   (unquote (car (cdr (car body))))
           (cond (unquote (cdr body))))))
   
  (defmacro -> (body)
    (cond
     (((empty? (cdr (cdr body)))
       (quasiquote ((unquote (car (car (cdr body))))
                    (unquote (car body))
                    (unquotesplicing (cdr (car (cdr body)))))))
      (true
       (quasiquote
        (-> (((unquote (car (car (cdr body))))
              (unquote (car body))
              (unquotesplicing (cdr (car (cdr body))))) (unquotesplicing (cdr (cdr body)))))))
      ))
   
    (-> (4 (+ 1) (- 2) (+ 4 5) (print " !")))
    )
  
  ;; -----
  )
