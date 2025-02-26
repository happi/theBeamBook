-module(beamfile).
-export([read/1]).

read(Filename) ->
    {ok, File} = file:read_file(Filename),
    <<"FOR1",
      Size:32/integer,
      "BEAM",
      Chunks/binary>> = File,
    {Size, parse_chunks(read_chunks(Chunks, []),[])}.

read_chunks(<<N,A,M,E, Size:32/integer, Tail/binary>>, Acc) ->
    %% Align each chunk on even 4 bytes
    ChunkLength = align_by_four(Size),
    <<Chunk:ChunkLength/binary, Rest/binary>> = Tail,
    read_chunks(Rest, [{[N,A,M,E], Size, Chunk}|Acc]);
read_chunks(<<>>, Acc) -> lists:reverse(Acc).

align_by_four(N) -> (4 * ((N+3) div 4)).

parse_chunks([{"Atom", _Size, <<_Numberofatoms:32/integer, Atoms/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{atoms,parse_atoms(Atoms)}|Acc]);
parse_chunks([{"AtU8", _Size, <<_Numberofatoms:32/integer, Atoms/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{atU8,parse_atoms(Atoms)}|Acc]);
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
    OpcodeSize = Size - SubSize - 8, %% 8 is size of ChunkSize & SubSize
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
parse_chunks([{"LitT", _ChunkSize,
              <<_CompressedTableSize:32, Compressed/binary>>}
             | Rest], Acc) ->
    <<_NumLiterals:32,Table/binary>> = zlib:uncompress(Compressed),
    Literals = parse_literals(Table),
    parse_chunks(Rest,[{literals,Literals}|Acc]);
parse_chunks([{"Abst", _ChunkSize, <<>>} | Rest], Acc) ->
    parse_chunks(Rest,Acc);
parse_chunks([{"Abst", _ChunkSize, <<AbstractCode/binary>>} | Rest], Acc) ->
    parse_chunks(Rest,[{abstract_code,binary_to_term(AbstractCode)}|Acc]);
parse_chunks([{"Line", _ChunkSize, <<LineTable/binary>>} | Rest], Acc) ->
    <<Ver:32,Bits:32,NumLineInstrs:32,NumLines:32,_NumFnames:32,
      Lines:NumLines/binary,Fnames/binary>> = LineTable,
    parse_chunks(Rest,[{line,
			[{version,Ver},
			 {bits,Bits},
			 {num_line_instrunctions,NumLineInstrs},
			 {lines,decode_lineinfo(binary_to_list(Lines),0)},
			 {function_names,Fnames}]}|Acc]);
parse_chunks([{"FunT", _Size, <<_Numberofentries:32/integer, Funs/binary>>}
                | Rest], Acc) ->
    parse_chunks(Rest,[{funs,parse_table(Funs)}|Acc]);
parse_chunks([{"Type", _Size, Chunk} | Rest], Acc) ->
    <<_Version:32/big, Count:32/big, TypeData/binary>> = Chunk,

    Types = parse_types(Count, TypeData, []),
    parse_chunks(Rest, [{type_info, Types}|Acc]);
parse_chunks([{"Meta", Size, Chunk} | Rest], Acc) ->
    <<MetaInfo:Size/binary, _Pad/binary>> = Chunk,
    Meta = binary_to_term(MetaInfo),
    parse_chunks(Rest,[{meta,Meta}|Acc]);

parse_chunks([Chunk|Rest], Acc) -> %% Not yet implemented chunk
    parse_chunks(Rest, [Chunk|Acc]);
parse_chunks([],Acc) -> Acc.

parse_atoms(<<Atomlength, Atom:Atomlength/binary, Rest/binary>>) when Atomlength > 0->
    [list_to_atom(binary_to_list(Atom)) | parse_atoms(Rest)];
parse_atoms(_Alignment) -> [].

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



-define(tag_i, 1).
-define(tag_a, 2).

decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a.

decode_int(Tag,B,Bs) when (B band 16#08) =:= 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag,N},Bs};
decode_int(Tag,B,[]) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3),
    {{Tag,N},[]};
decode_int(Tag,B,Bs) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    [B1|Bs1] = Bs,
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3) bor B1,
    {{Tag,N},Bs1};
decode_int(Tag,B,Bs) ->
    {Len,Bs1} = decode_int_length(B,Bs),
    {IntBs,RemBs} = take_bytes(Len,Bs1),
    N = build_arg(IntBs),
    {{Tag,N},RemBs}.

decode_lineinfo([B|Bs], F) ->
    Tag = decode_tag(B band 2#111),
    {{Tag,Num},RemBs} = decode_int(Tag,B,Bs),
    case Tag of
	i ->
	    [{F, Num} | decode_lineinfo(RemBs, F)];
	a ->
	    [B2|Bs2] = RemBs,
	    Tag2 = decode_tag(B2 band 2#111),
	    {{Tag2,Num2},RemBs2} = decode_int(Tag2,B2,Bs2),
	    [{Num, Num2} | decode_lineinfo(RemBs2, Num2)]
    end;
decode_lineinfo([],_) -> [].

decode_int_length(B, Bs) ->
    {B bsr 5 + 2, Bs}.


take_bytes(N, Bs) ->
    take_bytes(N, Bs, []).

take_bytes(N, [B|Bs], Acc) when N > 0 ->
    take_bytes(N-1, Bs, [B|Acc]);
take_bytes(0, Bs, Acc) ->
    {lists:reverse(Acc), Bs}.


build_arg(Bs) ->
    build_arg(Bs, 0).

build_arg([B|Bs], N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg([], N) ->
    N.


parse_types(_, <<>>, Acc) ->
    lists:reverse(Acc);
parse_types(-1, Type, Acc) ->
    lists:reverse([Type|Acc]);
parse_types(N, <<TypeBytes:16/binary, Rest/binary>>, Acc) ->
    %% See beam_disasm.erl and beam_types.erl to decode.
    parse_types(N-1, Rest, [TypeBytes|Acc]).
