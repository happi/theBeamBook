-module(show).
-export([ hex_tag/1
        , tag/1
        , tag_to_type/1
        ]).
-export([top_tag/1]).
-define(SMALL_BITS,
        case erlang:system_info(wordsize) of
            8 -> 60;  %% 64-bit VM ⇒ 64 − 4 tag bits
            4 -> 28   %% 32-bit VM ⇒ 32 − 4 tag bits
        end).

-define(MIN_SMALL, -(1 bsl (?SMALL_BITS-1))).   %% e.g. −2⁵⁹ on 64-bit
-define(MAX_SMALL,  (1 bsl (?SMALL_BITS-1)) - 1). %% e.g.  2⁵⁹−1 on 64-bit
%% Helper macros – valid for the ordinary 64-bit BEAM build
-define(PRIMARY_TAG_BITS, 2).
-define(TAG_IMMED1,      1).   % small int / header-less immediates
-define(TAG_LIST,        1).   % 4-bit layout when primary = 01
-define(TAG_BOXED,       2).   % boxed / pointer
-define(TAG_HEADER,      0).   % in-heap header word


%% Return {Primary, SubTag} describing the low-order bits
top_tag(Term) ->
    case Term of
        _ when is_integer(Term),  Term >= ?MIN_SMALL, Term =< ?MAX_SMALL ->
            {immed, small_int};

        _ when is_atom(Term)     -> {immed, atom};
        _ when is_boolean(Term)  -> {immed, bool};
        _ when Term =:= []       -> {immed, nil};
        _ when is_pid(Term)      -> {immed, pid};
        _ when is_port(Term)     -> {immed, port};
        _ when is_reference(Term)-> {immed, ref};

        [_|_]                    -> {cons_cell, list_ptr};     % TAG_LIST
        {_}                      -> {tuple_boxed, boxed_ptr};  % TAG_BOXED
        _ when is_float(Term)    -> {float_boxed, boxed_ptr};
        _ when is_binary(Term)   -> {bin_boxed, boxed_ptr};
        _ when is_map(Term)      -> {map_boxed, boxed_ptr};
        _ when is_function(Term) -> {fun_boxed, boxed_ptr};
        _                         -> {unknown, needs_native}
    end.

tag(Term) ->
  Bits = integer_to_list(erlang:system_info(wordsize)*8),
  FormatString = "~" ++ Bits ++ ".2.0B",
  io:format(FormatString,[top_tag(Term)]).

hex_tag(Term) ->
  Chars = integer_to_list(erlang:system_info(wordsize)*2),
  FormatString = "~" ++ Chars ++ ".16.0b",
  io:format(FormatString,[top_tag(Term)]).


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

