#!/usr/bin/env escript
%% Execute with the private OTP runtime and ERL_FLAGS='+S 1'.
-mode(compile).
main(_) ->
    "29" = erlang:system_info(otp_release),
    1 = erlang:system_info(schedulers_online),
    lists:foreach(fun(N) ->
        Expected = factorial(N), Expected = math:factorial(N)
    end, lists:seq(0,100) ++ [1000,5000,10000]),
    lists:foreach(fun(N) ->
        try math:factorial(N) of V -> error({expected_badarg,N,V})
        catch error:badarg -> ok end
    end, [-1,10001,1 bsl 100,1.0,atom,[]]),
    %% Repetition crosses both the immediate and allocated-result paths.
    lists:foreach(fun(_) ->
        1 = math:factorial(0), 5040 = math:factorial(7),
        Expected = factorial(1000), Expected = math:factorial(1000)
    end, lists:seq(1,1000)),
    {Outs,Gap} = scheduling_check(),
    io:format("Factorial values 0..100/1000/5000/10000, bounds and 1000 repeats passed.~n"
              "Observed ~p schedule-outs during factorial; largest observed slice ~p us.~n",
              [Outs,Gap]).
factorial(N) -> lists:foldl(fun erlang:'*'/2,1,lists:seq(1,N)).
scheduling_check() ->
    Parent = self(),
    P = spawn(fun() -> receive go -> ok end,
                      Value = math:factorial(10000),
                      Parent ! {result,self(),Value} end),
    1 = erlang:trace(P,true,[running,monotonic_timestamp]),
    P ! go,
    collect(P,undefined,0,0).
collect(P,In,Outs,Max) ->
    receive
        {trace_ts,P,in,_,At} -> collect(P,At,Outs,Max);
        {trace_ts,P,out,_,At} when is_integer(In) ->
            Gap = erlang:convert_time_unit(At-In,native,microsecond),
            collect(P,undefined,Outs+1,max(Max,Gap));
        {trace_ts,P,_,_,_} -> collect(P,In,Outs,Max);
        {result,P,V} ->
            V = factorial(10000),
            %% On one scheduler the other process cannot run without a yield.
            true = Outs > 1,
            {Outs,Max}
    after 10000 -> error(factorial_timeout)
    end.
