#!/usr/bin/env escript
%%! +S 2
-mode(compile).
main(_) ->
    "29" = erlang:system_info(otp_release),
    lists:foreach(fun(Name) ->
        File = "code/runtime_voice/" ++ Name ++ ".erl",
        {ok, Mod, Bin} = compile:file(File, [binary, warnings_as_errors]),
        {module, Mod} = code:load_binary(Mod, File, Bin)
    end, ["process_walk", "table_walk", "record_walk", "fun_walk"]),
    process_checks(), table_checks(), literal_checks(),
    {true, false, true} = record_walk:compare(),
    F = fun_walk:make(10,20),
    {{10,20},{env,[10,20]},4} = {F(),erlang:fun_info(F,env),erts_debug:flat_size(F)},
    io:format("PASS process signals/labels/iteration/limits, ETS lifetime, literal sharing, native-record observations~n").

process_checks() ->
    {P, Prio} = process_walk:start(),
    try
        P ! {work, one}, P ! {work, two},
        {messages, [{work,one},{work,two}]} = process_info(P, messages),
        [{work,one},{work,two}] = process_walk:drain(P),
        P ! {work, ordinary},
        ok = erlang:send(Prio, {work, urgent}, [priority]),
        [{work,urgent},{work,ordinary}] = process_walk:drain(P),
        {label,{book_worker,1}} = process_info(P,label),
        true = lists:member({P,{book_worker,1}}, process_walk:labels()),
        true = erlang:system_info(process_count) < erlang:system_info(process_limit)
    after
        normal = process_walk:stop(P)
    end,
    undefined = process_info(P,label),
    %% Iterator processing tolerates processes exiting before inspection.
    lists:foreach(fun(_) -> spawn(fun() -> ok end) end, lists:seq(1,50)),
    _ = process_walk:labels(), ok.

table_checks() ->
    T = table_walk:start(),
    try
        {set,protected,5} = {ets:info(T,type),ets:info(T,protection),ets:info(T,size)},
        [{3,[1,2,3]}] = ets:lookup(T,3),
        [{3,[1,2,3]}] = table_walk:reader(T),
        {error,badarg} = table_walk:writer(T),
        Payload = lists:seq(1,100),
        true = ets:insert(T,{6,Payload}),
        [{6,Copy}] = ets:lookup(T,6),
        false = erts_debug:same(Payload,Copy),
        true = Payload =:= Copy,
        true = ets:delete(T,6),
        [1,2,3,4,5] = table_walk:keys(T),
        false = ets:info(T,fixed),
        Ordered = ets:new(chunk_example,[ordered_set]),
        try
            true = ets:insert(Ordered,[{N,N} || N <- lists:seq(1,5)]),
            {[1,2,3],Cont} = ets:select(Ordered,[{{'$1','$2'},[],['$1']}],3),
            {[4,5],Last} = ets:select(Cont),
            '$end_of_table' = ets:select(Last)
        after true = ets:delete(Ordered)
        end
    after true = ets:delete(T)
    end,
    undefined = ets:info(T).

literal_checks() ->
    Key = {beam_book,make_ref()},
    try
        ok = persistent_term:put(Key,lists:seq(1,100)),
        A = persistent_term:get(Key), B = persistent_term:get(Key),
        true = erts_debug:same(A,B),
        true = persistent_term:erase(Key),
        100 = length(A)
    after persistent_term:erase(Key)
    end.
