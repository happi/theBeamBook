-module(show).
-export([ hex_tag/1
        , tag/1
        , tag_to_type/1
        ]).


tag(Term) ->
  Bits = integer_to_list(erlang:system_info(wordsize)*8),
  FormatString = "~" ++ Bits ++ ".2.0B",
  io:format(FormatString,[hipe_bifs:term_to_word(Term)]).

hex_tag(Term) ->
  Chars = integer_to_list(erlang:system_info(wordsize)*2),
  FormatString = "~" ++ Chars ++ ".16.0b",
  io:format(FormatString,[hipe_bifs:term_to_word(Term)]).


tag_to_type(Word) ->
  case Word band 2#11 of
    2#00 -> header;
    2#01 -> cons;
    2#10 -> boxed;
    2#11 ->
      case (Word bsr 2) band 2#11 of
        2#00 -> pid;
        2#01 -> port;
        2#10 ->
          case (Word bsr 4) band 2#11 of
            00 -> atom;
            01 -> 'catch';
            10 -> 'UNUSED';
            11 -> nil
          end;
        2#11 -> smallint
      end
  end.

