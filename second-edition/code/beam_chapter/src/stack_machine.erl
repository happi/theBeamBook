-module(stack_machine).
-export([compile/1, interpret/1]).

compile(String) ->
    [ParseTree] = element(2,
			  erl_parse:parse_exprs(
			    element(2,
				    erl_scan:string(String)))),
    generate_code(ParseTree).

generate_code({op, _Line, '+', Arg1, Arg2}) ->
    generate_code(Arg1) ++ generate_code(Arg2) ++ [add];
generate_code({op, _Line, '*', Arg1, Arg2}) ->
    generate_code(Arg1) ++ generate_code(Arg2) ++ [multiply];
generate_code({integer, _Line, I}) -> [push, I].

interpret(Code) -> interpret(Code, []).

interpret([push, I |Rest], Stack)              -> interpret(Rest, [I|Stack]);
interpret([add     |Rest], [Arg2, Arg1|Stack]) -> interpret(Rest, [Arg1+Arg2|Stack]);
interpret([multiply|Rest], [Arg2, Arg1|Stack]) -> interpret(Rest, [Arg1*Arg2|Stack]);
interpret([],              [Res|_])            -> Res.
