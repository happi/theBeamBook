-module(packet_demo).
-export([encode/1, decode/1, retag/2]).

encode(#{tag := Tag, payload := Payload})
  when is_integer(Tag), Tag >= 0, Tag =< 255, is_binary(Payload) ->
    <<Tag:8, Payload/binary>>.

decode(<<Tag:8, Payload/binary>>) ->
    #{tag => Tag, payload => Payload}.

retag(Packet, Tag) ->
    Packet#{tag := Tag}.
