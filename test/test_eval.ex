#(define symb 42)
#(define (f (lambda (a b) (+ a b c))) (f 1 2))
#(lambda (a b) (+ a b)(- b a)) => [:lambda, [:a, :b], [:+, :a, :b], [:-, :b, :a]]
#(+ 1 2, (+ 3 4)) => [:+, 1, 2, [:+, 3, 4]]

# [[:lambda,
#   [:a, :b],
#   [:+, :a, :b]],
#  1, 2]

# [[:lambda,
#   [:x, :y],
#   [[:lambda,
#     [:a, :b],
#     [:+, :a, :x]],
#    :x, :y]],
#  1, 2]

# 43 = Evaluator.eval([[:define,
# 											# bindings
# 											[:foo, 42,
# 											 :bar, [:lambda, [:a, :b], [:+, :a, :b]]],
# 											# code
# 											[:+, :foo, 1],
# 											[:bar, :foo, 1]]]) # invoke locally bound :bar, with :foo and 1 as paremeters

# 6 = Evaluator.eval([[:define,
# 										 # bindings
# 										 [:foo, 42,
# 											:bar, [:lambda, [:a, :b], [:+, :a, :b, 4]]],
# 										 # code
# 										 [:+, :foo, [:bar, 1, 2]],
										 
# 										 # nested :define
# 										 [[:define,
# 											 [:foo, 1,
# 												:neh, [:lambda, [:a, :f], [:f, :a, :a]]],
# 											 [:neh, :foo, :bar]
# 											]
# 										 ] # evaluate function returned by nested :define
# 										]
# 									 ] # evaluate function returned by nested :define
# 									)

# 44 =
#   Evaluator.eval([[:define,
# 									 # bindings
# 									 [:foo, 42,
# 										:bar, [:lambda, [:a, :b, :foo], [:+, :a, :b, :foo]]], # in lambda, :foo evaluates to the parameter value, *NOT* 42 (shadowing)
										 
# 										 # code
# 										 # nested :define
# 										 [[:define,
# 											 [:foo, 1,
# 												:neh, [:lambda, [:a, :f], [:f, :a, :a, :foo]]], # :foo evaluates to 42, *NOT* 1
# 												 [:neh, :foo, :bar] # :foo evaluates to 1 (shadowing)
# 											]
# 										 ] # evaluate function returned by nested :define
# 									]
# 								 ] # evaluate function returned by nested :define
# 								)

