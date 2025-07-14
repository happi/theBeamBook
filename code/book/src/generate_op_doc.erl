-module(generate_op_doc).

-export([docbook/2, from_shell/1]).

-record(op, {name="", arity="", opcode, doc="", spec="", deprecated=false}).

from_shell([In, Out]) ->
    docbook(atom_to_list(In), atom_to_list(Out)),
    halt().

docbook(InFile, OutFile) ->
    {ok, File} = file:open(InFile, [read]),
    Ops = parse(File),
    Doc = docbook_format(Ops),
    file:write_file(OutFile, Doc).



docbook_format(Ops) ->
    docbook_format_line(lists:reverse(lists:keysort(2,Ops)))
        ++
        "|=================================================\n".

docbook_format_line([#op{name=Name,
                        arity=Arity,
                        opcode=Opcode,
                        doc=Doc,
                        spec=Spec,
                        deprecated=Deprecated}|
                        Prev]) ->
    docbook_format_line(Prev) ++
        "|" ++ format_name(Name, Deprecated) ++
        "|" ++ strip(Arity) ++
        "|" ++ format_opcode(Opcode, Deprecated) ++
        "|" ++ format_spec(Spec, Deprecated) ++
        "|" ++ strip(Doc) ++ "\n";
docbook_format_line([]) ->
    "=== Generic Instructions\n" ++ 
    "[options=" ++ [$"] ++ "header" ++ [$"] ++ "]\n"
        "|=================================================\n" ++
        "| Name | Arity | Op Code | Spec | Documentation\n".


format_name(Name, Deprecated) ->
    if
        Deprecated -> "[line-through]#" ++ strip(Name) ++ "#";
        true -> strip(Name)
    end.

format_spec([Name|Args], false) ->
    "*"++strip(Name)++"*" ++ " "
        ++ string:join([format_arg(A) || A<-Args], ", ");
format_spec(_, true) -> "*DEPRECATED*";
format_spec([], _) -> "".

format_arg(A) -> "_"++strip(A)++"_".

format_opcode(undefined, _Deprecated) ->
    "";
format_opcode(Opcode, Deprecated) ->
    if
        Deprecated -> "(";
        true -> ""
    end ++
        strip(Opcode) ++
        if
            Deprecated -> ")";
            true -> ""
        end.



strip(S) ->
 [ escape(Char)
  || Char <- string:strip(S, right, 10)].

escape(Char) ->
    case Char of
        $| -> "\|";
        $\n -> " ";
        _ -> Char
    end.

parse(File) ->
    parse(File, #op{}, []).

parse(File, Op, Ops) ->
    case file:read_line(File) of
        {ok, Line} ->
            {NewOp, NewOps} = parse_line(Line, Op, Ops),
            parse(File,  NewOp, NewOps);
        eof -> case Op#op.name of
                   "" -> Ops;
                   _ -> [Op|Ops]
               end
    end.

parse_line("##" ++ Rest, Op, Ops) ->
    {parse_doc(Rest, Op), Ops};
parse_line("#" ++ _, Op, Ops) -> {Op, Ops};
parse_line([N|_]=Line, Op, Ops) when N >= $0, N =< $9 ->
    [OpNo, NA] = string:tokens(Line, ":"),
    [Name, Arity] = string:tokens(string:strip(NA, left, $ ), "/"),
    NewOp =
        case Name of
            "-" ++ OpName ->
                Op#op{name=OpName, deprecated=true};
            _ ->
                Op#op{name=Name}
        end,
    {#op{}, [NewOp#op{opcode=OpNo, arity=Arity} | Ops]};
parse_line(_, Op, Ops) ->
    {Op, Ops}.

parse_doc(Line, Op) ->
    case string:tokens(Line, " ") of
        ["@spec" | Rest] -> Op#op{spec=Rest};
        ["@doc" | Rest] -> Op#op{doc=string:join(Rest, " ")};
        _ -> Op#op{doc=Op#op.doc++Line}
    end.
