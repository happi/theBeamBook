-module(json_parser).
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    json(AST, []).

-define(FUNCTION(Clauses), {function, Label, Name, Arity, Clauses}).

%% We are only interested in code inside functions.
json([?FUNCTION(Clauses) | Elements], Res) ->
    json(Elements, [?FUNCTION(json_clauses(Clauses)) | Res]);
json([Other|Elements], Res) -> json(Elements, [Other | Res]);
json([], Res) -> lists:reverse(Res).

%% We are interested in the code in the body of a function.
json_clauses([{clause, CLine, A1, A2, Code} | Clauses]) ->
    [{clause, CLine, A1, A2, json_code(Code)} | json_clauses(Clauses)];
json_clauses([]) -> [].


-define(JSON(Json), {bin, _, [{bin_element
                                          , _
                                          , {tuple, _, [Json]}
                                          , _
                                          , _}]}).

%% We look for: <<"json">> = Json-Term
json_code([])                     -> [];
json_code([?JSON(Json)|MoreCode]) -> [parse_json(Json) | json_code(MoreCode)];
json_code(Code)                   -> Code.

%% Json Object -> [{}] | [{Label, Term}]
parse_json({tuple,Line,[]})            -> {cons, Line, {tuple, Line, []}};
parse_json({tuple,Line,Fields})        -> parse_json_fields(Fields,Line);
%% Json Array -> List
parse_json({cons, Line, Head, Tail})   -> {cons, Line, parse_json(Head),
                                                       parse_json(Tail)};
parse_json({nil, Line})                -> {nil, Line};
%% Json String -> <<String>>
parse_json({string, Line, String})     -> str_to_bin(String, Line);
%% Json Integer -> Integer
parse_json({integer, Line, Integer})   -> {integer, Line, Integer};
%% Json Float -> Float
parse_json({float, Line, Float})       -> {float, Line, Float};
%% Json Constant -> true | false | null
parse_json({atom, Line, true})         -> {atom, Line, true};
parse_json({atom, Line, false})        -> {atom, Line, false};
parse_json({atom, Line, null})         -> {atom, Line, null};

%% Variables, should contain Erlang encoded Json
parse_json({var, Line, Var})         -> {var, Line, Var};
%% Json Negative Integer or Float
parse_json({op, Line, '-', {Type, _, N}}) when Type =:= integer
                                               ; Type =:= float ->
                                          {Type, Line, -N}.
%% parse_json(Code)                  -> io:format("Code: ~p~n",[Code]), Code.

-define(FIELD(Label, Code), {remote, L, {string, _, Label}, Code}).

parse_json_fields([], L) -> {nil, L};
%% Label : Json-Term  --> [{<<Label>>, Term} | Rest]
parse_json_fields([?FIELD(Label, Code) | Rest], _) ->
    cons(tuple(str_to_bin(Label, L), parse_json(Code), L)
         , parse_json_fields(Rest, L)
         , L).


tuple(E1, E2, Line)    -> {tuple, Line, [E1, E2]}.
cons(Head, Tail, Line) -> {cons, Line, Head, Tail}.

str_to_bin(String, Line) ->
    {bin
     , Line
     , [{bin_element
         , Line
         , {string, Line, String}
         , default
         , default
        }
       ]
    }.
