-module(beamfile).
-export([read/1]).

%% Educational reader for trusted BEAM files. Atom decoding creates atoms.
read(Filename) ->
    {ok, <<"FOR1", Size:32, "BEAM", Chunks/binary>>} = file:read_file(Filename),
    Size = byte_size(Chunks) + 4,
    {Size, parse_chunks(read_chunks(Chunks, []), [])}.

read_chunks(<<Name:4/binary, Size:32, Tail/binary>>, Acc) ->
    Padding = (4 - Size rem 4) rem 4,
    <<Chunk:Size/binary, _:Padding/binary, Rest/binary>> = Tail,
    read_chunks(Rest, [{binary_to_list(Name), Size, Chunk}|Acc]);
read_chunks(<<>>, Acc) -> lists:reverse(Acc).

parse_chunks([{Name, _Size, <<Count:32/signed, Atoms/binary>>}|Rest], Acc)
  when Name =:= "Atom"; Name =:= "AtU8" ->
    Encoding = case Name of "Atom" -> latin1; "AtU8" -> utf8 end,
    LengthFormat = case Count < 0 of true -> compact; false -> byte end,
    parse_chunks(Rest, [{atoms, parse_atoms(abs(Count), Atoms,
                                           Encoding, LengthFormat)}|Acc]);
parse_chunks([{"ExpT", _Size,
              <<_Numberofentries:32/integer, Exports/binary>>}
             | Rest], Acc) ->
    parse_chunks(Rest,[{exports,parse_table(Exports)}|Acc]);
parse_chunks([{"ImpT", _Size,
              <<_Numberofentries:32/integer, Imports/binary>>}
             | Rest], Acc) ->
    parse_chunks(Rest,[{imports,parse_table(Imports)}|Acc]);
parse_chunks([{"Code", Size, <<SubSize:32/integer, Chunk/binary>>} | Rest], Acc) ->
    <<Info:SubSize/binary, Code/binary>> = Chunk,
    OpcodeSize = Size - SubSize - 4, %% Chunk size excludes the chunk header
    <<OpCodes:OpcodeSize/binary, _Align/binary>> = Code,
    parse_chunks(Rest,[{code,parse_code_info(Info), OpCodes}|Acc]);
parse_chunks([{"StrT", _Size, <<Strings/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{strings,binary_to_list(Strings)}|Acc]);
parse_chunks([{"Attr", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    Attribs = binary_to_term(Bin),
    parse_chunks(Rest,[{attributes,Attribs}|Acc]);
parse_chunks([{"CInf", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    CInfo = binary_to_term(Bin),
    parse_chunks(Rest,[{compile_info,CInfo}|Acc]);
parse_chunks([{"LocT", _Size,
              <<_Numberofentries:32/integer, Locals/binary>>}
             | Rest], Acc) ->
    parse_chunks(Rest,[{locals,parse_table(Locals)}|Acc]);
parse_chunks([{"LitT", _Size, <<InflatedSize:32, Data/binary>>}|Rest], Acc) ->
    Table = case InflatedSize of
        0 -> Data;
        _ -> Inflated = zlib:uncompress(Data),
             InflatedSize = byte_size(Inflated),
             Inflated
    end,
    <<Count:32, Entries/binary>> = Table,
    Literals = parse_literals(Entries),
    Count = length(Literals),
    parse_chunks(Rest, [{literals, Literals}|Acc]);
parse_chunks([{"Dbgi", _Size, Data}|Rest], Acc) when Data =/= <<>> ->
    {debug_info_v1, Backend, Info} = binary_to_term(Data),
    parse_chunks(Rest, [{debug_info, Backend, Info}|Acc]);
parse_chunks([{"Abst", _ChunkSize, <<>>} | Rest], Acc) ->
    parse_chunks(Rest,Acc);
parse_chunks([{"Abst", _ChunkSize, <<AbstractCode/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{abstract_code,binary_to_term(AbstractCode)}|Acc]);
parse_chunks([{"Line", _Size,
              <<0:32, Bits:32, NumInstructions:32, NumLines:32, NumNames:32,
                Data/binary>>}|Rest], Acc) ->
    {Lines, NamesData} = parse_lines(NumLines, 0, Data, []),
    Names = parse_names(NumNames, NamesData),
    parse_chunks(Rest, [{line, [{version, 0}, {bits, Bits},
                               {num_line_instructions, NumInstructions},
                               {lines, Lines}, {file_names, Names}]}|Acc]);
parse_chunks([{"FunT", _Size, <<_Numberofentries:32/integer, Funs/binary>>}
                | Rest], Acc) ->
    parse_chunks(Rest,[{funs,parse_funs(Funs)}|Acc]);
parse_chunks([{"Type", _Size, <<4:32, Count:32, Data/binary>>}|Rest], Acc) ->
    parse_chunks(Rest, [{type_info, parse_types(Count, Data)}|Acc]);
parse_chunks([{"Meta", Size, Chunk} | Rest], Acc) ->
    <<MetaInfo:Size/binary, _Pad/binary>> = Chunk,
    Meta = binary_to_term(MetaInfo),
    parse_chunks(Rest,[{meta,Meta}|Acc]);

parse_chunks([Chunk|Rest], Acc) -> %% Not yet implemented chunk
    parse_chunks(Rest, [Chunk|Acc]);
parse_chunks([],Acc) -> lists:reverse(Acc).

parse_atoms(0, <<>>, _Encoding, _Format) -> [];
parse_atoms(Count, Data, Encoding, Format) when Count > 0 ->
    {Length, Tail} = case Format of
        byte -> <<N:8, Bs/binary>> = Data, {N, Bs};
        compact -> {0, N, Bs} = compact(Data), {N, Bs}
    end,
    <<Name:Length/binary, Rest/binary>> = Tail,
    [binary_to_atom(Name, Encoding)|parse_atoms(Count-1, Rest, Encoding, Format)].

%% Decode a nonnegative compact operand, retaining its three-bit tag.
compact(<<B, Rest/binary>>) when B band 8 =:= 0 ->
    {B band 7, B bsr 4, Rest};
compact(<<B, B1, Rest/binary>>) when B band 16 =:= 0 ->
    {B band 7, ((B band 16#e0) bsl 3) bor B1, Rest};
compact(<<B, Tail/binary>>) ->
    {Length, Bytes} = case B bsr 5 of
        7 -> {0, N, Bs} = compact(Tail), {N+9, Bs};
        N -> {N+2, Tail}
    end,
    <<Value:Length/unit:8, Rest/binary>> = Bytes,
    {B band 7, Value, Rest}.

parse_table(<<Function:32/integer,
                Arity:32/integer,
                Label:32/integer,
                Rest/binary>>) ->
    [{Function, Arity, Label} | parse_table(Rest)];
parse_table(<<>>) -> [].


parse_code_info(<<Instructionset:32/integer,
		  OpcodeMax:32/integer,
		  NumberOfLabels:32/integer,
		  NumberOfFunctions:32/integer,
		  Rest/binary>>) ->
    [{instructionset, Instructionset},
     {opcodemax, OpcodeMax},
     {numberoflabels, NumberOfLabels},
     {numberofFunctions, NumberOfFunctions} |
     case Rest of
	 <<>> -> [];
	 _ -> [{newinfo, Rest}]
     end].

parse_literals(<<Size:32,Literal:Size/binary,Tail/binary>>) ->
    [binary_to_term(Literal) | parse_literals(Tail)];
parse_literals(<<>>) -> [].



%% Each FunT entry contains six words, unlike import/export/local tables.
parse_funs(<<Name:32, Arity:32, Label:32, Index:32, Free:32, OldUniq:32,
             Rest/binary>>) ->
    [{Name, Arity, Label, Index, Free, OldUniq}|parse_funs(Rest)];
parse_funs(<<>>) -> [].

parse_lines(0, _File, Rest, Acc) -> {lists:reverse(Acc), Rest};
parse_lines(Count, File, Data, Acc) ->
    case compact(Data) of
        {2, NextFile, Rest} -> parse_lines(Count, NextFile, Rest, Acc);
        {1, Line, Rest} ->
            parse_lines(Count-1, File, Rest, [{File, Line}|Acc])
    end.

parse_names(0, <<>>) -> [];
parse_names(Count, <<Length:16, Name:Length/binary, Rest/binary>>) when Count > 0 ->
    [unicode:characters_to_list(Name)|parse_names(Count-1, Rest)].

%% Preserve each version-4 entry as bytes for inspection with beam_types:decode_ext/1.
%% Other versions remain raw chunks in parse_chunks/2.
parse_types(0, <<>>) -> [];
parse_types(Count, <<Bits:16, Tail/binary>>) when Count > 0 ->
    ExtraSize = 8 * ((Bits bsr 13) band 1) +
                8 * ((Bits bsr 14) band 1) + ((Bits bsr 15) band 1),
    <<Extra:ExtraSize/binary, Rest/binary>> = Tail,
    [<<Bits:16, Extra/binary>>|parse_types(Count-1, Rest)].
