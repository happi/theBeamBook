#!/usr/bin/env escript
%%! +S 2
-mode(compile).
main(_) ->
    "29" = erlang:system_info(otp_release),
    Source = "code/beam_chapter/packet_demo/packet_demo.erl",
    {ok,packet_demo,Binary,[]} = compile:file(Source,[binary,return_errors,return_warnings]),
    {module,packet_demo} = code:load_binary(packet_demo,Source,Binary),
    Map = #{tag=>7,payload=><<1,2,3>>},
    <<7,1,2,3>> = packet_demo:encode(Map),
    Map = packet_demo:decode(<<7,1,2,3>>),
    Updated = packet_demo:retag(Map,9),
    <<9,1,2,3>> = packet_demo:encode(Updated),
    7 = maps:get(tag,Map),
    #{tag:=9,payload:=<<1,2,3>>,extra:=kept} =
        packet_demo:retag(Map#{extra=>kept},9),
    <<0>> = packet_demo:encode(#{tag=>0,payload=><<>>}),
    <<255>> = packet_demo:encode(#{tag=>255,payload=><<>>}),
    #{tag:=255,payload:=<<>>} = packet_demo:decode(<<255>>),
    lists:foreach(fun(M) ->
        expect_error(function_clause,fun() -> packet_demo:encode(M) end)
    end,[#{tag=>-1,payload=><<>>},#{tag=>256,payload=><<>>},
         #{tag=>7.0,payload=><<>>},#{tag=>7,payload=>not_binary},
         #{tag=>7,payload=><<1:1>>},#{tag=>7},not_map]),
    lists:foreach(fun(B) ->
        expect_error(function_clause,fun() -> packet_demo:decode(B) end)
    end,[<<>>,<<7,1:1>>,not_binary]),
    expect_error({badkey,tag},fun() -> packet_demo:retag(#{},9) end),
    expect_error({badmap,not_map},fun() -> packet_demo:retag(not_map,9) end),
    %% The map update accepts 256; the encoder's guard enforces the byte range.
    expect_error(function_clause,fun() ->
        packet_demo:encode(packet_demo:retag(Map,256))
    end),
    io:format("PASS packet/map flow, byte boundaries, guard failures, binary mismatch, badkey and badmap~n"),
    {ok,packet_demo,{packet_demo,_,_,_,Functions,_},[]} =
        compile:file(Source,[to_asm,binary,return_errors,return_warnings]),
    {function,encode,1,_,Encode} = lists:keyfind(encode,2,Functions),
    {function,decode,1,_,Decode} = lists:keyfind(decode,2,Functions),
    {function,retag,2,_,Retag} = lists:keyfind(retag,2,Functions),
    {get_map_elements,{f,1},_,
        {list,[{atom,tag},{x,2},{atom,payload},{x,1}]}} =
        lists:keyfind(get_map_elements,1,Encode),
    true = lists:member({test,is_integer,{f,1},[{x,2}]},Encode),
    true = lists:member({test,is_binary,{f,1},[{x,1}]},Encode),
    2 = length([ok || {test,is_ge,_,_} <- Encode]),
    {bs_create_bin,{f,0},0,3,8,{x,0},{list,Segments}} =
        lists:keyfind(bs_create_bin,1,Encode),
    [{atom,integer},1,1,nil,{tr,{x,2},{t_integer,{0,255}}},{integer,8},
     {atom,binary},2,8,nil,{tr,{x,1},{t_bitstring,8,false}},{atom,all}] = Segments,
    {test,bs_start_match3,{f,3},1,[{x,0}],{x,1}} =
        lists:keyfind(bs_start_match3,2,[I || I <- Decode,is_tuple(I),tuple_size(I)>=2]),
    {bs_match,{f,5},{x,1},
        {commands,[{ensure_at_least,8,8},
                   {integer,2,{literal,[]},8,1,{x,0}},
                   {get_tail,2,8,{x,1}}]}} = lists:keyfind(bs_match,1,Decode),
    {put_map_assoc,{f,0},{literal,#{}},{x,0},2,
        {list,[{atom,payload},{x,1},{atom,tag},{x,0}]}} =
        lists:keyfind(put_map_assoc,1,Decode),
    {put_map_exact,{f,0},{x,0},{x,0},2,{list,[{atom,tag},{x,1}]}} =
        lists:keyfind(put_map_exact,1,Retag),
    io:format("PASS OTP29 emitted guards, typed operands, binary matching/construction, map lookup/update~n").
expect_error(Reason,F) ->
    try F() of V -> error({expected_error,Reason,got,V})
    catch error:Reason -> ok end.
