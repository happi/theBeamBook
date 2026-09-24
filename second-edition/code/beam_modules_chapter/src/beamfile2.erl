-module(beamfile2).
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
parse_chunks([Chunk|Rest], Acc) ->
    parse_chunks(Rest, [Chunk|Acc]);
parse_chunks([], Acc) -> lists:reverse(Acc).

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
