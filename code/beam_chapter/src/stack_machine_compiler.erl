-module(stack_machine_compiler).
-export([compile/2]).

compile(Expression, FileName) ->
    [ParseTree] = element(2,
			  erl_parse:parse_exprs(
			    element(2,
				    erl_scan:string(Expression)))),
    file:write_file(FileName, generate_code(ParseTree) ++ [stop()]).

generate_code({op, _Line, '+', Arg1, Arg2}) -> 
    generate_code(Arg1) ++ generate_code(Arg2) ++ [add()];
generate_code({op, _Line, '*', Arg1, Arg2}) -> 
    generate_code(Arg1) ++ generate_code(Arg2) ++ [multiply()];
generate_code({integer, _Line, I}) -> [push(), integer(I)].

stop()     -> 0.
add()      -> 1.
multiply() -> 2.
push()     -> 3.
integer(I) ->    
    L = binary_to_list(binary:encode_unsigned(I)),
    [length(L) | L].
    
