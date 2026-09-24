#!/usr/bin/env escript
%%! +S 2
-mode(compile).

main([]) ->
    "29" = erlang:system_info(otp_release),
    Root = filename:dirname(filename:dirname(filename:dirname(escript:script_name()))),
    Tmp = filename:join("/tmp", "bb71-foundations-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Tmp),
    try
        json_checks(Root, Tmp),
        load_checks(),
        heap_checks(),
        chunk_checks(Root, Tmp),
        operand_checks(),
        io:format("PASS foundations checks on OTP 29~n")
    after
        {ok, Names} = file:list_dir(Tmp),
        [file:delete(filename:join(Tmp, N)) || N <- Names],
        file:del_dir(Tmp)
    end.

json_checks(Root, Tmp) ->
    {ok,json_parser,PT} = compile:file(filename:join(Root,"code/compiler_chapter/src/json_parser.erl"),[binary,warnings_as_errors]),
    {module,json_parser} = code:load_binary(json_parser,"json_parser.erl",PT),
    Source = "-module(bb71_json).\n-compile({parse_transform,json_parser}).\n-export([empty/0,nested/0,full/0]).\nempty()-><<{{}}>>.\nnested()-><<{{\"child\":{}}}>>.\nfull()-><<{{\"n\":42,\"items\":[1,2]}}>>.\n",
    Path=filename:join(Tmp,"bb71_json.erl"), ok=file:write_file(Path,Source),
    {ok,bb71_json,B}=compile:file(Path,[binary,warnings_as_errors]),
    {module,bb71_json}=code:load_binary(bb71_json,Path,B),
    [{}]=bb71_json:empty(), [{<<"child">>,[{}]}]=bb71_json:nested(),
    [{<<"n">>,42},{<<"items">>,[1,2]}]=bb71_json:full(),
    io:format("PASS empty, nested and populated JSON transform~n").

forms(M) ->
    [{attribute,1,module,M},{attribute,2,export,[{f,0},{wait,0}]},
     {function,3,f,0,[{clause,3,[],[],[{string,3,"hello"}]}]},
     {function,4,wait,0,[{clause,4,[],[],[{'receive',4,[{clause,4,[{atom,4,stop}],[],[{atom,4,ok}]}]}]}]}].

load_checks() ->
    {ok,bb71_a,A}=compile:forms(forms(bb71_a),[binary]),
    {ok,bb71_b,B}=compile:forms(forms(bb71_b),[binary]),
    ok=erlang:finish_loading([erlang:prepare_loading(bb71_a,A),erlang:prepare_loading(bb71_b,B)]),
    "hello"=bb71_a:f(), "hello"=bb71_b:f(),
    P=spawn(bb71_a,wait,[]),
    wait_waiting(P),
    {module,bb71_a}=code:load_binary(bb71_a,"v2",A),
    true=erlang:check_old_code(bb71_a), false=code:soft_purge(bb71_a),
    M=monitor(process,P), P!stop, receive {'DOWN',M,process,P,normal}->ok after 2000->error(process_timeout) end,
    true=erlang:check_old_code(bb71_a), true=code:soft_purge(bb71_a),
    false=erlang:check_old_code(bb71_a),
    [ModuleAttr|CFs]=forms(bb71_c),
    {ok,bb71_c,C}=compile:forms([ModuleAttr,{attribute,1,on_load,{f,0}}|CFs],[binary]),
    {ok,bb71_d,D}=compile:forms(forms(bb71_d),[binary]),
    try erlang:finish_loading([erlang:prepare_loading(bb71_c,C),erlang:prepare_loading(bb71_d,D)]) of
        _ -> error(on_load_batch_accepted)
    catch error:system_limit -> ok end,
    io:format("PASS atomic loading, on_load restriction and explicit old-code purge~n").

wait_waiting(P) ->
    case process_info(P,status) of {status,waiting}->ok; _->timer:sleep(1),wait_waiting(P) end.

heap_checks() ->
    Dead=spawn(fun()->ok end), M=monitor(process,Dead),
    receive {'DOWN',M,process,Dead,_}->ok after 2000->error(dead_timeout) end,
    []=[{Dead,W} || {total_heap_size,W}<-[process_info(Dead,total_heap_size)]],
    [spawn(fun()->timer:sleep(1) end) || _<-lists:seq(1,1000)],
    Obs=lists:keysort(2,[{P,W} || P<-processes(),{total_heap_size,W}<-[process_info(P,total_heap_size)]]),
    true=lists:all(fun({P,W})->is_pid(P) andalso is_integer(W) end,Obs),
    io:format("PASS process-exit tolerant heap observations~n").

chunk_checks(Root,Tmp) ->
    {ok,bb71_chunks,B}=compile:forms(forms(bb71_chunks),[binary]),
    {ok,bb71_chunks,Cs}=beam_lib:all_chunks(B), {"StrT",<<>>}=lists:keyfind("StrT",1,Cs),
    {"LitT",<<0:32,1:32,Size:32,Term:Size/binary>>}=lists:keyfind("LitT",1,Cs),
    "hello"=binary_to_term(Term),
    Path=filename:join(Tmp,"bb71_compressed.erl"),
    ok=file:write_file(Path,"-module(bb71_compressed).\n-export([f/0]).\nf()->ok.\n"),
    {ok,bb71_compressed}=compile:file(Path,[compressed,{outdir,Tmp}]),
    {ok,<<16#1f,16#8b,_/binary>>=Gz}=file:read_file(filename:join(Tmp,"bb71_compressed.beam")),
    <<"FOR1",_/binary>>=zlib:gunzip(Gz),
    {ok,beamfile,R}=compile:file(filename:join(Root,"code/beam_modules_chapter/src/beamfile.erl"),[binary]),
    {module,beamfile}=code:load_binary(beamfile,"beamfile.erl",R),
    ReaderPath=filename:join(Tmp,"beamfile.beam"),ok=file:write_file(ReaderPath,R),
    {_Size,Decoded}=beamfile:read(ReaderPath), true=is_list(Decoded),
    io:format("PASS StrT/LitT, gzip wrapper and complete-reader entry point~n").

operand_checks() ->
    <<16#12>>=iolist_to_binary([beam_asm:encode(2,1)]), % atom index 1
    <<0>>=iolist_to_binary([beam_asm:encode(0,0)]), % unsigned zero
    [begin <<Byte>>=iolist_to_binary([beam_asm:encode(7,N)]) end || {N,Byte} <- [{1,16#17},{2,16#27},{3,16#37},{4,16#47},{5,16#57}]],
    io:format("PASS compact operand tag golden bytes~n").
