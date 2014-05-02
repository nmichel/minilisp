defmodule Tokenizer do
  def tokens(path) do
		Enum.reduce(File.stream!(path, [], :line), [], fn l, acc ->
																												tokens(l, acc)
																									 end)
		|> Enum.reverse
  end
  defp tokens("", acc) do
		acc
	end
  defp tokens(e, acc) do
		[h | [t]] = Regex.run(re, e, [capture: :all_but_first])
		tokens(t, [Atomizer.atom(h) | acc])
	end
	defp re do
		~r/\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)/ #"
  end
end

defmodule Atomizer do
	def atom("+") do :+ end # Corner cases, because binary_to_integer("+") => 0
	def atom("-") do :- end # same
	def atom(e = <<?", t::binary>>) do #"
    l = byte_size(t)-1
		<<s :: [size(l), binary], "\"">> = t
		s
	end
	def atom(e) do
		try do
			binary_to_float(e)
		catch 
			_, _ ->
				try do
					binary_to_integer(e)
				catch
					_, _ ->
						binary_to_atom(e)
				end
		end
	end
end

defmodule Parser do
	def parse(tokens) do
		case parse(tokens, []) do
			{[], acc} ->
				acc
			{t, acc} ->
				parse(t) ++ acc
		end
	end
	defp parse([], acc) do
		{[], Enum.reverse(acc)}
	end
	defp parse([:'(' | t], acc) do
		{rem, ast} = parse_sexp(t, [])
		parse(rem, [Enum.reverse(ast) | acc])
	end
	defp parse(e = [:')' | t], acc) do
		{e, acc}
	end
	defp parse([e | t], acc) do
		{t, [e | acc]}
	end

	defp parse_sexp([], acc) do
		{[], acc}
	end
	defp parse_sexp(t, acc) do
		{rem, ast} = parse(t, acc)
		case rem do
			[:')' | r] ->
				{r, ast}
			_ ->
				parse_sexp(rem, ast)
		end
	end
end

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

defmodule Evaluator do
  def root_env do
    [+: fn [h] -> h
           [h|t] -> Enum.reduce(t, h, &Kernel.+/2)
        end,
     -: fn [h] -> -h
           [h|t] -> Enum.reduce(t, h, &(&2 - &1))
        end,
     and: fn [a|t] ->
               Enum.reduce(t, a, &Kernel.and/2)
          end,
     or: fn [a|t] ->
              Enum.reduce(t, a, &Kernel.or/2)
         end,
     list: fn p ->
                p
           end,
     list?: fn [x] when is_list(x) ->
                 true
               [_] ->
                 false
            end,
     empty?: fn [[]] ->
                  true
                [[_h|_t]]->
                  false
             end,
     car: fn [[h|t]] ->
               h
          end,
     cdr: fn [[h|t]] ->
               t
          end,
     ==: fn [a,b|t] ->
              a == b
         end,
     if: fn [c, t, e | _] ->
              case c do
                true -> t
                _ -> e
              end
         end,
     print: fn p ->
                 IO.puts [inspect(p)]
            end
    ]
    |> Enum.reduce(Env.new, fn({k, v}, e) when is_atom(k) ->
                                Env.bind(e, k, v)
                            end)
  end

  def eval(e) do
    {r, _} = eval(e, root_env)
    r
  end

  def eval(e, env) when is_boolean(e) do # true and false are boolean AND atom
    {e, env}
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
               for e <- el do Evaluator.eval(e, loc_env) end
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
                                                {r, _} = Evaluator.eval(b, env)
                                                Env.bind(acc, k, r)
                                            end)
             [{r, _} | _] = 
               for e <- code do Evaluator.eval(e, loc_env) end
               |> Enum.reverse
             r
        end
    {f, env}
  end
  def eval([h | t], env) do
    {f, loc_env} = Evaluator.eval(h, env)
    ps =
      for p <- t do eval(p, loc_env) end
      |> Enum.map fn {v, _} -> v end
    r = Evaluator.apply(f, ps)
    {r, env}
  end
  def eval(c, env) do
    {c, env}
  end

  def apply(f, p) do
    Kernel.apply(f, [p])
  end
end

defmodule Minilisp do
	def play(path) do
		tokens = Tokenizer.tokens(path)
		Parser.parse(tokens)
		|> Enum.reduce(nil, fn(e, _) ->
														Evaluator.eval(e)
												end)
		
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

43 = Evaluator.eval([[:define,
											# bindings
											[:foo, 42,
											 :bar, [:lambda, [:a, :b], [:+, :a, :b]]],
											# code
											[:+, :foo, 1],
											[:bar, :foo, 1]]]) # invoke locally bound :bar, with :foo and 1 as paremeters

6 = Evaluator.eval([[:define,
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
  Evaluator.eval([[:define,
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
