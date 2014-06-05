(defmacro cond (body)
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

  (defmacro ->> (form)
    (if (empty? (cdr (cdr form)))
        `(~@(car (cdr form)) ~(car form))
        `(->> ((->> (~(car form) ~(car (cdr form))))
               ~@(cdr (cdr form))))
        )

    (print "->> " (->> ((->> (4 (+ 3 2 1))) (list "coucou "))))
    (print "->> " (->> (4 (+ 3 2 1) (list "coucou "))))

    ;; (print "->> pass : "
    ;;        (if
    ;;            (== (list 4 3 1 2)
    ;;                (->> ((list 1 2) (cons 3) (cons 4))))
    ;;            true
    ;;            false))

    ;; (->>
    ;;  (
    ;;   (print "if-ize " (if-ize (false 42 "42")))
    ;;   (print "unless " (unless (== true false) 42))
        
    ;;   (defmacro unless (c form)
    ;;     `(if-ize ((== false ~c) ~form nil))
    ;;     )

    ;;   (defmacro if-ize (form)
    ;;     `(if ~(car form)
    ;;          ~(car (cdr form))
    ;;          ~@(cdr (cdr form)))
    ;;     )
    ;;   )
    ;;  )
    ;; )
  
    (print "sugar quote: " '(+ 42 0))
    (print "sugar quote quote: " ''(+ 42 0))
    (print "sugar quote unquote: " '~(+ 42 0))
    (print "sugar quote unquotesplicing: " '~@(+ 42 0))
    (print "sugar quote quasiquote: " '`(+ 42 0))
  )
)
