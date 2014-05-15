(defmacro ->> (form)
  (if (empty? (cdr (cdr form)))
      `(~@(car (cdr form)) ~(car form))
      `(->> ((->> (~(car form) ~(car (cdr form))))
	     ~@(cdr (cdr form))))
      )
      
  (print "->> pass : "
	 (if
	     (== (list 4 3 1 2)
		 (->> ((list 1 2) (cons 3) (cons 4))))
	         true
	   false))
  )
