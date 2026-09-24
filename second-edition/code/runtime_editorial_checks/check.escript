#!/usr/bin/env escript
%%! +S 2
-mode(compile).

main([]) ->
    "29" = erlang:system_info(otp_release),
    load("code/memory_chapter/src/gc_example.erl"),
    load("code/runtime_editorial_checks/listlen.erl"),
    load("code/runtime_editorial_checks/arith_typed.erl"),
    load("code/runtime_editorial_checks/dispatch.erl"),
    run("GC roots, runtime allocation, and preserved sharing", fun gc/0),
    run("bounded arithmetic and list-length behavior", fun arithmetic/0),
    run("dispatch figure instruction sequence", fun dispatch/0),
    run("ETS patterns, guards, complete chunk traversal and cleanup", fun ets_examples/0),
    run("binary-copy ownership and literal sharing", fun memory/0),
    run("atomic unique sequence allocation", fun sequence/0),
    ok.

load(Path) ->
    {ok, M, B} = compile:file(Path, [binary, warnings_as_errors]),
    {module, M} = code:load_binary(M, Path, B).
run(Name, F) -> F(), io:format("PASS ~s~n", [Name]).

instructions(Path, Name, Arity) ->
    {ok, _, {_, _, _, _, Functions, _}} = compile:file(Path, [to_asm, binary]),
    {function, Name, Arity, _, Is} = lists:keyfind(Name, 2,
        [F || F = {function, _, A, _, _} <- Functions, A =:= Arity]),
    Is.

gc() ->
    [A, B] = R = gc_example:example(<<"Hello">>),
    "Hello" = A,
    true = erts_debug:same(A, B),
    14 = erts_debug:size(R),
    24 = erts_debug:flat_size(R),
    Random = crypto:strong_rand_bytes(19),
    [C, D] = gc_example:example(Random),
    true = erts_debug:same(C, D),
    C = binary_to_list(Random),
    Path = "code/memory_chapter/src/gc_example.erl",
    Gen = instructions(Path, gen_data, 1),
    true = lists:member({call_ext,1,{extfunc,erlang,binary_to_list,1}}, Gen),
    true = lists:any(fun({put_tuple2,_,_}) -> true; (_) -> false end, Gen),
    Example = instructions(Path, example, 1),
    %% Selection must overwrite the tuple before a tail call; no retained Y root.
    [{get_tuple_element,{x,0},0,{x,0}},{call_last,1,_,0}] =
        lists:dropwhile(fun(I) -> I =/= {get_tuple_element,{x,0},0,{x,0}} end, Example),
    Collect = instructions(Path, collect, 1),
    true = lists:member({move,{x,0},{y,0}}, Collect),
    true = lists:member({call_ext,0,{extfunc,erlang,garbage_collect,0}}, Collect).

arithmetic() ->
    3 = listlen:len([a,b,c]), 0 = listlen:len([]),
    0 = arith_typed:sum_pair({0,0}),
    1999998 = arith_typed:sum_pair({999999,999999}),
    lists:foreach(fun(X) ->
        try arith_typed:sum_pair(X) of _ -> error(accepted_invalid_range)
        catch error:function_clause -> ok end
    end, [{-1,1},{1000000,1},{1.0,2},{1 bsl 100,0}]).

dispatch() ->
    {1,3,1,42} = dispatch:run(1,2),
    %% The figure in the JIT chapter shows exactly this straight-line run.
    [{gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,1}},
     {move,{integer,42},{x,3}},
     {move,{x,0},{x,2}},
     {call_only,4,{f,_}}] =
        lists:dropwhile(fun({gc_bif,_,_,_,_,_}) -> false; (_) -> true end,
                        instructions("code/runtime_editorial_checks/dispatch.erl", run, 2)).

ets_examples() ->
    MS = [{{'$1', '$2'}, [{'>', '$2', 997}], ['$1']}],
    CMS = ets:match_spec_compile(MS),
    {true,false} = {ets:is_compiled_ms(CMS),ets:is_compiled_ms(MS)},
    [1] = ets:match_spec_run([{1,999},{2,1}],CMS),
    T = ets:new(chunk_example,[ordered_set]),
    true = ets:insert(T,[{N,N} || N <- lists:seq(1,5)]),
    {[1,2,3], Cont} = ets:select(T,[{{'$1','$2'},[],['$1']}],3),
    {[4,5], Last} = ets:select(Cont),
    '$end_of_table' = ets:select(Last),
    true = ets:delete(T),
    undefined = ets:info(T),
    H = ets:new(hash_example,[set,public]),
    try
        true = ets:insert(H,{key,[a,b,c]}),
        [{key,[a,b,c]}] = ets:match_object(H,{key,'_'}),
        [key] = ets:select(H,[{{'$1','$2'},[{'==',{length,'$2'},3}],['$1']}]),
        try
            ets:safe_fixtable(H,true),
            try error(simulated_traversal_failure)
            after ets:safe_fixtable(H,false) end
        catch error:simulated_traversal_failure -> ok end,
        false = ets:info(H,safe_fixed)
    after ets:delete(H) end.

memory() ->
    Payload = binary:copy(<<7>>,10000),
    Slice = binary:part(Payload,17,100),
    Small = binary:copy(Slice),
    10000 = binary:referenced_byte_size(Slice),
    100 = binary:referenced_byte_size(Small),
    Slice = Small,
    Key = {?MODULE,make_ref()},
    persistent_term:put(Key,{lists:seq(1,20)}),
    try
        Lit = persistent_term:get(Key),
        Parent = self(),
        spawn(fun() -> Parent ! {literal,persistent_term:get(Key)} end),
        receive {literal,Other} -> true = erts_debug:same(Lit,Other) end,
        T = ets:new(literal_copy,[]),
        try
            true = ets:insert(T,{key,Lit}),
            [{key,Copy}] = ets:lookup(T,key),
            false = erts_debug:same(Lit,Copy),
            Lit = Copy
        after ets:delete(T) end,
        persistent_term:put(Key,Lit),
        true = erts_debug:same(Lit,persistent_term:get(Key))
    after persistent_term:erase(Key) end,
    persistent_term:put(Key,enabled),
    enabled = persistent_term:get(Key),
    true = persistent_term:erase(Key).

sequence() ->
    Ref = atomics:new(1,[]), Parent = self(),
    Pids = [spawn(fun() ->
        Parent ! {self(),[atomics:add_get(Ref,1,1) || _ <- lists:seq(1,500)]}
    end) || _ <- lists:seq(1,4)],
    Values = lists:append([receive {P,Vs} -> Vs end || P <- Pids]),
    ValuesSorted = lists:sort(Values),
    ValuesSorted = lists:seq(1,2000).
