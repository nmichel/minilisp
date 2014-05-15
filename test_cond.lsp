(defmacro cond (body)
  ;; Expand a list of (cond, code) pair into a structure of nested "if/else" forms.
   
  (if (empty? body)
      (quasiquote true)
      (quasiquote
       (if (unquote (car (car body)))
           (unquote (car (cdr (car body))))
	   (cond (unquote (cdr body))))))
   
  (cond (((== 1 2) (list 1 2 3))
         ((== 3 1) (list 4 5 6))
         ((== 1 0) (list 7 8 9))
         (true (list "a" "b" "c"))))
  )

