-module(function_deps).

-export([get/3]).

-spec 'get'(atom(), string(), non_neg_integer()) -> [{atom(), atom()}].
'get'(Module, Function, Arity) ->
  ErlPath = atom_to_list(Module) ++ ".erl",
  BeamPath = outdir(Module) ++ "/" ++  atom_to_list(Module) ++ ".beam",

  ok = ktn_code:beam_to_erl(BeamPath, ErlPath),
  {ok, Src} = file:read_file(ErlPath),

  Root = ktn_code:parse_tree(Src),
  FindFunPred =
    fun
      (Zipper) ->
        case zipper:node(Zipper) of
          #{type := function,
            attrs := #{name := FunName,
                       arity := FunArity}} ->
            FunName == Function andalso FunArity == Arity;
          _ ->
            false
        end
    end,

  case find_zipper(FindFunPred, Root) of
    [] ->
      not_found;
    [FunctionNode] ->
      FunCallPred =
        fun
          (Zipper) ->
            case zipper:node(Zipper) of
              #{type := call} -> true;
              _ -> false
            end
        end,
      Calls = find_zipper(FunCallPred, FunctionNode),
      lists:map(fun module_function/1, Calls)
  end.

module_function(#{type := call,
                  attrs := #{function := Function}}) ->
  module_function(Function);
module_function(#{type := remote,
                  attrs := #{function := #{attrs := #{value := Function}},
                             module := #{attrs := #{value := Module}}}}) ->
  {Module, Function};
module_function(#{type := remote,
                  attrs := #{function := #{attrs := #{value := Function}},
                             module := #{attrs := #{name := Module}}}}) ->
  {Module, Function};
module_function(#{type := remote,
                  attrs := #{function := #{attrs := #{name := Function}},
                             module := #{attrs := #{name := Module}}}}) ->
  {Module, Function};
module_function(#{type := remote,
                  attrs := #{function := #{attrs := #{name := Function}},
                             module := #{attrs := #{value := Module}}}}) ->
  {Module, Function};
module_function(#{type := atom,
                  attrs := #{value := Name}}) ->
  Name;
module_function(#{type := var,
                  attrs := #{name := Name}}) ->
  Name.


outdir(Module) ->
  ModuleInfo = Module:module_info(),
  CompileInfo = proplists:get_value(compile, ModuleInfo),
  OptsInfo = proplists:get_value(options, CompileInfo),
  proplists:get_value(outdir, OptsInfo).


-spec find_zipper(fun((zipper:zipper()) -> boolean()), ktn_code:tree_node()) ->
                     [ktn_code:tree_node()].
find_zipper(Pred, Root) ->
  IsBranch = fun
               (#{content := [_ | _]}) -> true;
               (_) -> false
             end,
  Children = fun (#{content := Content}) -> Content end,
  MakeNode = fun(Node, _) -> Node end,
  Zipper = zipper:new(IsBranch, Children, MakeNode, Root),
  Results = find_zipper(Pred, Zipper, []),
  lists:reverse(Results).

find_zipper(Pred, Zipper, Results) ->
  case zipper:is_end(Zipper) of
    true ->
      Results;
    false ->
      Node = zipper:node(Zipper),
      NewResults = case Pred(Zipper) of
                     true -> [Node | Results];
                     false -> Results
                   end,
      find_zipper(Pred, zipper:next(Zipper), NewResults)
  end.
