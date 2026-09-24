#!/usr/bin/env escript
%%! +S 2
-mode(compile).

main([]) ->
    Root = filename:dirname(filename:dirname(escript:script_name())),
    Out = filename:join(Root, "ebin"),
    ok = filelib:ensure_dir(filename:join(Out, "placeholder")),
    [begin
         {ok, Mod, Bin} = compile:file(filename:join([Root, "src", Name ++ ".erl"]),
                                       [binary, warnings_as_errors]),
         {module, Mod} = code:load_binary(Mod, Name ++ ".erl", Bin)
     end || Name <- ["beamfile1", "beamfile2", "beamfile"]],
    Unicode = list_to_atom([16#1f680, 16#e5]),
    Long = list_to_atom(lists:duplicate(255, 16#1f680)),
    ShortForms = forms([Unicode, '']),
    [begin {ok, reader_fixture, Bin} = compile:forms(Fs, [binary, debug_info|Opts]),
     check_beam(Out, Bin) end
     || {Fs, Opts} <- [{forms([Unicode, '', Long]), []},
                       {forms([Unicode, '', Long]), [compressed_literals]},
                       {ShortForms, [no_long_atoms]},
                       {ShortForms, [no_long_atoms, compressed_literals]}]],
    {ok, reader_fixture, Base} = compile:forms(forms([plain, '']), [binary, debug_info]),
    check_latin1(Out, Base),
    check_types(Out, Base),
    check_lines(Out, Base),
    {module, reader_fixture} = code:load_binary(reader_fixture, "fixture.erl", Base),
    {literal, [1,2,3], #{answer := 42}} = reader_fixture:value(),
    false = lists:member(reader_fixture, int:interpreted()),
    {t_tuple, 0, false, #{}} = element(1, beam_types:decode_ext(
        beam_types:encode_ext({t_tuple, 3, true, #{}}))),
    io:format("PASS: reader round trips (4 compiler modes), legacy Latin-1, "
              "Unicode/long/empty atoms, indices, exact chunk padding, literals, "
              "Code, FunT, Line, Type v4/unknown, debug metadata and tuple erasure~n").

forms(Atoms) ->
    [{attribute, 1, module, reader_fixture},
     {attribute, 1, file, {"fixture.erl", 1}},
     {attribute, 2, export, [{value,0}, {atoms,0}, {closure,1}, {bounded,1}] ++
                            [{A,0} || A <- Atoms]},
     {function, 3, value, 0,
      [{clause, 3, [], [], [erl_parse:abstract({literal,[1,2,3],#{answer => 42}})]}]},
     {function, 4, atoms, 0, [{clause, 4, [], [], [erl_parse:abstract(Atoms)]}]},
     {function, 5, closure, 1,
      [{clause, 5, [{var,5,'X'}], [],
        [{'fun',5,{clauses,[{clause,5,[],[],[{var,5,'X'}]}]}}]}]},
     {function, 3000, bounded, 1,
      [{clause,3000,[{var,3000,'X'}],[],
        [{op,3000,'band',{var,3000,'X'},{integer,3000,255}}]}]}] ++
    [{function,6,A,0,[{clause,6,[],[],[{atom,6,A}]}]} || A <- Atoms].

check_beam(Out, Bin) ->
    File = filename:join(Out, "reader_fixture.beam"),
    ok = file:write_file(File, Bin),
    {ok, reader_fixture, Raw} = beam_lib:all_chunks(Bin),
    {_, Framed} = beamfile1:read(File),
    Raw = [{Name, Bytes} || {Name, Size, Bytes} <- Framed, byte_size(Bytes) =:= Size],
    {ok, {reader_fixture, [{atoms, Indexed}]}} = beam_lib:chunks(Bin, [atoms]),
    Atoms = [Atom || {_, Atom} <- Indexed],
    [{1,reader_fixture}|_] = Indexed,
    {_, Small} = beamfile2:read(File),
    Atoms = proplists:get_value(atoms, Small),
    {_, Full} = beamfile:read(File),
    Atoms = proplists:get_value(atoms, Full),
    {ok, {reader_fixture, [{exports, Exports}, {imports, Imports},
                           {attributes, Attributes}, {compile_info, CompileInfo}]}} =
        beam_lib:chunks(Bin, [exports, imports, attributes, compile_info]),
    Exports = lists:sort([{lists:nth(I, Atoms), A}
                          || {I,A,_} <- proplists:get_value(exports, Full)]),
    Imports = lists:sort([{lists:nth(M, Atoms), lists:nth(F, Atoms), A}
                          || {M,F,A} <- proplists:get_value(imports, Full)]),
    Attributes = proplists:get_value(attributes, Full),
    CompileInfo = proplists:get_value(compile_info, Full),
    <<Sub:32, _:Sub/binary, Code/binary>> = proplists:get_value("Code", Raw),
    {code, _, Code} = lists:keyfind(code, 1, Full),
    <<NumFuns:32, FunBytes/binary>> = proplists:get_value("FunT", Raw),
    Funs = proplists:get_value(funs, Full),
    NumFuns = length(Funs),
    FunBytes = << <<N:32,A:32,L:32,I:32,F:32,U:32>> || {N,A,L,I,F,U} <- Funs >>,
    <<Inflated:32, LitBytes/binary>> = proplists:get_value("LitT", Raw),
    LitData = case Inflated of 0 -> LitBytes; _ -> zlib:uncompress(LitBytes) end,
    <<Count:32, Entries/binary>> = LitData,
    Literals = proplists:get_value(literals, Full),
    Count = length(Literals),
    Literals = terms(Entries),
    true = lists:member({literal,[1,2,3],#{answer => 42}}, Literals),
    {debug_info_v1, Backend, Data} = binary_to_term(proplists:get_value("Dbgi", Raw)),
    {debug_info, Backend, Data} = lists:keyfind(debug_info, 1, Full),
    <<4:32, NumTypes:32, TypeBytes/binary>> = proplists:get_value("Type", Raw),
    Types = proplists:get_value(type_info, Full),
    NumTypes = length(Types),
    TypeBytes = iolist_to_binary(Types),
    [begin {_, <<>>} = beam_types:decode_ext(T) end || T <- Types],
    Full.

terms(<<N:32, T:N/binary, Rest/binary>>) -> [binary_to_term(T)|terms(Rest)];
terms(<<>>) -> [].

replace_chunk(Bin, Old, New, Payload) ->
    {ok, reader_fixture, Chunks} = beam_lib:all_chunks(Bin),
    {ok, Rebuilt} = beam_lib:build_module(
        [case Name of Old -> {New, Payload}; _ -> {Name, Bytes} end
         || {Name, Bytes} <- Chunks]),
    Rebuilt.

read_rebuilt(Out, Bin) ->
    File = filename:join(Out, "reader_synthetic.beam"),
    ok = file:write_file(File, Bin),
    {_, Read} = beamfile:read(File),
    {File, Read}.

check_latin1(Out, Base) ->
    {ok, {reader_fixture, [{atoms, Indexed}]}} = beam_lib:chunks(Base, [atoms]),
    Atoms = [A || {_, A} <- Indexed] ++ [list_to_atom([16#e5]), ''],
    Data = iolist_to_binary([<<(length(Atoms)):32>>|
        [begin B = atom_to_binary(A, latin1), <<(byte_size(B)):8, B/binary>> end
         || A <- Atoms]]),
    Bin = replace_chunk(Base, "AtU8", "Atom", Data),
    {File, Full} = read_rebuilt(Out, Bin),
    {_, Small} = beamfile2:read(File),
    Atoms = proplists:get_value(atoms, Small),
    Atoms = proplists:get_value(atoms, Full),
    {ok, {reader_fixture, [{atoms, Expected}]}} = beam_lib:chunks(Bin, [atoms]),
    Atoms = [A || {_, A} <- Expected].

check_types(Out, Base) ->
    %% Every optional-field combination, with signed bounds and nonzero units.
    Entries = [iolist_to_binary([<<(32 bor (Flags bsl 13)):16>>,
                  case Flags band 1 of 0 -> <<>>; _ -> <<-17:64/signed>> end,
                  case Flags band 2 of 0 -> <<>>; _ -> <<255:64/signed>> end,
                  case Flags band 4 of 0 -> <<>>; _ -> <<7>> end])
               || Flags <- lists:seq(0,7)],
    Types = iolist_to_binary(Entries),
    {_, Read} = read_rebuilt(Out, replace_chunk(Base, "Type", "Type", <<4:32,8:32,Types/binary>>)),
    Entries = proplists:get_value(type_info, Read),
    [begin {_, <<>>} = beam_types:decode_ext(E) end || E <- Entries],
    Unknown = <<99:32, 1:32, 42>>,
    {_, Other} = read_rebuilt(Out, replace_chunk(Base,"Type","Type",Unknown)),
    {"Type",9,Unknown} = lists:keyfind("Type",1,Other).

check_lines(Out, Base) ->
    %% File switches do not count as line items. Exercise every compact width.
    Lines = [{0,15},{0,16},{1,2047},{1,2048},{2,1 bsl 72}],
    Encoded = iolist_to_binary([ [beam_asm:encode(2,F),beam_asm:encode(1,L)]
                                || {F,L} <- Lines]),
    Name1 = unicode:characters_to_binary([16#e5] ++ ".hrl"),
    Name2 = <<"other.hrl">>,
    Chunk = <<0:32, 0:32, 5:32, 5:32, 2:32, Encoded/binary,
              (byte_size(Name1)):16, Name1/binary,
              (byte_size(Name2)):16, Name2/binary>>,
    {_, Read} = read_rebuilt(Out, replace_chunk(Base,"Line","Line",Chunk)),
    Info = proplists:get_value(line, Read),
    Lines = proplists:get_value(lines, Info),
    [NameChars, "other.hrl"] = proplists:get_value(file_names, Info),
    NameChars = unicode:characters_to_list(Name1).
