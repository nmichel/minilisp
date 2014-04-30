defmodule Env do
  def new(parent \\ nil) do
    %{parent: parent}
  end
  
  def bind(env, name, val) do
    Map.put(env, name, val)
  end
  
  def fetch(env, key) do
    r = Map.get(env, key)
    case {r, Map.get(env, :parent)} do
      {nil, nil} ->
        nil
      {nil, m} ->
        fetch(m, key)
      {v, _} ->
        v
    end
  end
end

defmodule Lisp do
  def root_env do
    [+: fn [h] -> h
           [h|t] -> Enum.reduce(t, h, &Kernel.+/2)
        end,
     -: fn [h] -> -h
           [h|t] -> Enum.reduce(t, h, &(&2 - &1))
        end]
    |> Enum.reduce(Env.new, fn({k, v}, e) when is_atom(k) ->
                                Env.bind(e, k, v)
                            end)
  end

  def eval(e) do
    {r, _} = eval(e, root_env)
    r
  end

  def eval(c, env) when is_atom(c) do
    {Env.fetch(env, c), env}
  end
  def eval([:lambda | t], env) do
    f = fn rp ->
             [fp | el] = t
             # Build a new environment with formal param names bound to concrete values
             # 
             loc_env = 
               Enum.zip(fp, rp)
               |> Enum.reduce(Env.new(env), fn({k, v}, acc) ->
                                                Env.bind(acc, k, v)
                                            end)
             # Evaluation of the function body
             # lambda return value is the evaluation value of the last expression
             # 
             [{r, _} | _] = 
               for e <- el do Lisp.eval(e, loc_env) end
               |> Enum.reverse
             r
        end
    {f, env}
  end
  def eval([:define, bindings | code], env) do
    f = fn _ ->
             loc_env =
               Enum.chunk(bindings, 2)
               |> Enum.reduce(Env.new(env), fn([k, b], acc) when is_atom(k) ->
                                                {r, _} = Lisp.eval(b, env)
                                                Env.bind(acc, k, r)
                                            end)
             [{r, _} | _] = 
               for e <- code do Lisp.eval(e, loc_env) end
               |> Enum.reverse
             r
        end
    {f, env}
  end
  def eval([h | t], env) do
    {f, loc_env} = Lisp.eval(h, env)
    ps =
      for p <- t do eval(p, loc_env) end
      |> Enum.map fn {v, _} -> v end
    r = Lisp.apply(f, ps)
    {r, env}
  end
  def eval(c, env) do
    {c, env}
  end

  def apply(f, p) do
    Kernel.apply(f, [p])
  end
end

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

43 = Lisp.eval([[:define,
                 # bindings
                 [:foo, 42,
                  :bar, [:lambda, [:a, :b], [:+, :a, :b]]],
                 # code
                 [:+, :foo, 1],
                 [:bar, :foo, 1]]]) # invoke locally bound :bar, with :foo and 1 as paremeters

6 = Lisp.eval([[:define,
                # bindings
                [:foo, 42,
                 :bar, [:lambda, [:a, :b], [:+, :a, :b, 4]]],
                # code
                [:+, :foo, [:bar, 1, 2]],
                
                # nested :define
                [[:define,
                  [:foo, 1,
                   :neh, [:lambda, [:a, :f], [:f, :a, :a]]],
                  [:neh, :foo, :bar]
                 ]
                ] # evaluate function returned by nested :define
               ]
              ] # evaluate function returned by nested :define
             )

44 =
  Lisp.eval([[:define,
              # bindings
              [:foo, 42,
               :bar, [:lambda, [:a, :b, :foo], [:+, :a, :b, :foo]]], # in lambda, :foo evaluates to the parameter value, *NOT* 42 (shadowing)
              
              # code
              # nested :define
              [[:define,
                [:foo, 1,
                 :neh, [:lambda, [:a, :f], [:f, :a, :a, :foo]]], # :foo evaluates to 42, *NOT* 1
                [:neh, :foo, :bar] # :foo evaluates to 1 (shadowing)
               ]
              ] # evaluate function returned by nested :define
             ]
            ] # evaluate function returned by nested :define
           )
