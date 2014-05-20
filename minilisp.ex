defmodule Tokenizer do
  def tokens(path) do
		Enum.reduce(File.stream!(path, [], :line), [], fn l, acc ->
																												tokens(l, acc)
																									 end)
		|> Enum.reverse
  end
  def tokens("", acc) do
		acc
	end
  def tokens(e, acc) do
		[h | [t]] = Regex.run(re, e, [capture: :all_but_first])
		acc = 
			case h do
				"" ->
					acc
				<<";",  _::bitstring>> ->
					acc
				_ ->
					[Atomizer.atom(h) | acc]
			end
		tokens(t, acc)
	end
	defp re do
		~r/\s*(~@|[('`~)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`~;)]*)(.*)/ #"
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
	defp to_quote(:'`') do :quasiquote end
	defp to_quote(:'~') do :unquote end
	defp to_quote(:'~@') do :unquotesplicing end
	defp to_quote(:"'") do :quote end

	def parse(tokens) do
		case parse(tokens, []) do
			{[], acc} ->
				acc
      {t, acc} ->
        IO.puts ["remaining tokens: ", inspect(t)]
        acc
		end
	end
	defp parse([], acc) do
		{[], acc}
	end
	defp parse([q, :'(' | t], acc) when q == :'`' or q == :'~' or q == :'~@' or q == :"'" do
		{rem, ast} = parse_sexp(t, [])
		parse(rem, acc ++ [[to_quote(q), ast]])
	end
	defp parse([q | t ], acc) when q == :'`' or q == :'~' or q == :'~@' or q == :"'" do
		{rem, [ast]} = parse(t, []) # Beware of the [ast] (*not* ast)
		parse(rem, acc ++ [[to_quote(q), ast]])
	end
	defp parse([:'(' | t], acc) do
		{rem, ast} = parse_sexp(t, [])
		parse(rem, acc ++ [ast])
	end
  defp parse(e = [:')' | t], acc) do
    {e, acc}
  end
  defp parse([e | t], acc) do
    {t, acc ++ [e]}
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
  
  def bind(env, name, val, tag \\ []) do
    Map.put(env, name, {val, tag})
  end

  def fetch_with_tags(env, key) do
    r = Map.get(env, key)
    case {r, Map.get(env, :parent)} do
      {nil, nil} ->
        nil
      {nil, m} ->
        fetch_with_tags(m, key)
      {v, _} ->
        v
    end
  end
  
  def fetch(env, key) do
    r = fetch_with_tags(env, key)
    case r do
      nil ->
        nil
      {v, tags} ->
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
						 x ->
							 nil
          end,
     cdr: fn [[h|t]] ->
               t
						 x ->
							 nil
          end,
     ==: fn [a,b] ->
              a == b
         end,
		 cons: fn [h, t] when is_list(t) ->
								[h] ++ t
							[h, t] ->
								[h] ++ [t]
					 end,
		 append: fn [h, t] when is_list(t) ->
									h ++ t
								[h, t] ->
									h ++ [t]
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
    eval(e, root_env)
  end

  def expand(e) do
    expand(e, root_env)
  end

  def eval(e, env) when is_boolean(e) do # true and false are boolean AND atom
    e
  end
  def eval(c, env) when is_atom(c) do
    Env.fetch(env, c)
  end
  def eval([:quote, e], env) do
    e
  end
  def eval([:lambda | t], env) do
    fn rp ->
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
         for e <- el do Evaluator.eval(e, loc_env) end
         |> Enum.reverse
         |> List.first
    end
  end
  def eval([:define, bindings | code], env) do
    fn _ ->
         loc_env =
           Enum.chunk(bindings, 2)
           |> Enum.reduce(Env.new(env), fn([k, b], acc) when is_atom(k) ->
                                            Env.bind(acc, k, eval(b, env))
                                        end)
         for e <- code do Evaluator.eval(e, loc_env) end
         |> Enum.reverse
         |> List.first
    end
  end
  def eval([:if, c, t, e], env) do
    if Evaluator.eval(c, env) do
      Evaluator.eval(t, env)
    else
      Evaluator.eval(e, env)
    end
  end
  def eval([h | t], env) do
    f = eval(h, env)
    ps = for p <- t do eval(p, env) end
    Evaluator.apply(f, ps)
  end
  def eval(c, env) do
    c
  end

  def apply(f, p) do
    Kernel.apply(f, [p])
  end
  
  def expand(e = [:quote | t], env) do
    e
  end
  def expand([:lambda, p | body], env) do
    ebody = for e <- body do expand(e, env) end
    [:lambda, p | ebody]
  end
  def expand([:define, bindings | body], env) do
    ebindings = 
      Enum.chunk(bindings, 2)
      |> Enum.map(fn([k, b]) ->
                      true = is_atom(k)
                      [k, expand(b, env)]
                  end)
      |> Enum.reverse
      |> Enum.reduce([], fn([k, eb], acc) ->
                             [k, eb | acc]
                         end)
    ebody = for e <- body do expand(e, env) end
    [:define, ebindings | ebody]
  end
  def expand([:if, c, t, e], env) do
    [:if | for i <- [c, t, e] do expand(i, env) end]
  end
  def expand([:defmacro, name, params, body | code], env) do
    ebody = expand(body, env)
    mf = [:lambda, params, ebody]
    p = eval(mf, env)
    local_env = Env.bind(Env.new(env), name, p, [macro: true])
    expand([[:lambda, [] | code]], local_env)
  end
  def expand([:quasiquote, e], env) do
    expand_quasiquote(e)
  end
  def expand(e = [f | args], env) when is_atom(f) do
    case Env.fetch_with_tags(env, f) do
      {m, tags} ->
        case Keyword.fetch(tags, :macro) do
          {:ok, true} ->
						v = Evaluator.apply(m, args)
            expand(v, env)
          _ ->
            for x <- e do expand(x, env) end
        end
      nil ->
        for x <- e do expand(x, env) end
    end
  end
  def expand(e, env) when is_list(e) do
    for x <- e do expand(x, env) end
  end
  def expand(e, env) do
    e
  end

  def expand_quasiquote([:unquote, e]) do
    e
  end
  def expand_quasiquote([[:unquotesplicing, e] | t]) do
    [:append, e, expand_quasiquote(t)]
  end
  def expand_quasiquote(e = [h | t]) do
    he = expand_quasiquote(h)
		te = expand_quasiquote(t)
    [:cons, he, te]
  end
  def expand_quasiquote(e) do
    [:quote, e]
  end
end

defmodule Minilisp do
	def play(path) do
		tokens = Tokenizer.tokens(path)
		Parser.parse(tokens)
		|> Enum.reduce(nil, fn(e, _) ->
                            ast = Evaluator.expand(e)
														Evaluator.eval(ast)
												end)
		
	end
end

