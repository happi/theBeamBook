-module(beamfile1).
-export([read/1]).

read(Filename) ->
   {ok, File} = file:read_file(Filename),
   <<"FOR1",
     Size:32/integer,
     "BEAM",
     Chunks/binary>> = File,
   {Size, read_chunks(Chunks, [])}.

read_chunks(<<Name:4/binary, Size:32, Tail/binary>>, Acc) ->
    Padding = (4 - Size rem 4) rem 4,
    <<Chunk:Size/binary, _:Padding/binary, Rest/binary>> = Tail,
    read_chunks(Rest, [{binary_to_list(Name), Size, Chunk}|Acc]);
read_chunks(<<>>, Acc) -> lists:reverse(Acc).
