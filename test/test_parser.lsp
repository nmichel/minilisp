(+ 1 2 3)

(- 3 2 1)

`42
(quasiquote 42)

(quasiquote (- 3 2 1))
`(- 1 2 3)

`(~(car '(1 2 3)) ~@(cdr '(1 2 3)))

'`(~(car '(1 2 3)) ~@(cdr '(1 2 3)))

`(+ ~@'(1 2 3))
