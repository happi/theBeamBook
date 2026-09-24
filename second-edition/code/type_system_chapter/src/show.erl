% tag::classification[]
%%%-------------------------------------------------------------------
%%%  show.erl  - schematic BEAM tag classification in pure Erlang
%%%
%%%  Infers categories using predicates; does not inspect machine words.
%%%  PID/port classification assumes that an identifier with our node name
%%%  belongs to the current node incarnation. Native records are unsupported.
%%%
%%%  The tag layout follows erts/emulator/beam/erl_term.h at OTP 29:
%%%
%%%    primary (bits 1..0)   00 header  01 cons  10 boxed  11 immediate
%%%    immed1  (bits 3..2)   00 pid     01 port  10 immed2 11 small int
%%%    immed2  (bits 5..4)   00 atom    01 catch 10 unused 11 nil
%%%-------------------------------------------------------------------
-module(show).

-export([top_tag/1,               %% -> {primary, kind}
         tag/1, hex_tag/1,        %% -> zero-padded string of the low tag bits
         tag_to_type/1]).         %% -> header | cons | boxed | pid | ...

%%--------------------------------------------------------------------
%% Constants that depend on the VM word size
%%--------------------------------------------------------------------
small_limits() ->
    case erlang:system_info(wordsize) of
        8 -> {-(1 bsl 59), (1 bsl 59) - 1};   %% 64-bit build: 60 payload bits
        4 -> {-(1 bsl 27), (1 bsl 27) - 1}    %% 32-bit build: 28 payload bits
    end.

word_bits()  -> erlang:system_info(wordsize) * 8.
hex_digits() -> erlang:system_info(wordsize) * 2.

%%--------------------------------------------------------------------
%% High-level tag classifier within the supported subset above.
%% Boolean values use the atom clause, just like all other atoms.
%%--------------------------------------------------------------------
top_tag(Term) when is_integer(Term) ->
    {Min, Max} = small_limits(),
    if  Term >= Min, Term =< Max -> {immed, small_int};
        true                     -> {boxed, bignum}
    end;
top_tag([])                           -> {immed, nil};
top_tag(Term) when is_atom(Term)      -> {immed, atom};
top_tag(Term) when is_pid(Term) ->
    case node(Term) =:= node() of
        true  -> {immed, pid};             %% local pid: one tagged word
        false -> {boxed, external_pid}     %% remote pid: heap object
    end;
top_tag(Term) when is_port(Term) ->
    case node(Term) =:= node() of
        true  -> {immed, port};
        false -> {boxed, external_port}
    end;
top_tag(Term) when is_reference(Term) -> {boxed, ref};
top_tag([_|_])                        -> {cons, list};
top_tag(Term) when is_tuple(Term)     -> {boxed, tuple};
top_tag(Term) when is_float(Term)     -> {boxed, float};
top_tag(Term) when is_bitstring(Term) -> {boxed, bitstring};
top_tag(Term) when is_map(Term)       -> {boxed, map};
top_tag(Term) when is_function(Term)  -> {boxed, 'fun'}.

%%--------------------------------------------------------------------
%% Pretty-print helpers (zero-padded to the word size)
%%--------------------------------------------------------------------
tag(Term) ->
    pad_left(integer_to_list(tag_word(Term), 2), word_bits(), $0).

hex_tag(Term) ->
    pad_left(string:uppercase(integer_to_list(tag_word(Term), 16)),
             hex_digits(), $0).

pad_left(Str, Width, PadChar) ->
    PadCnt = Width - length(Str),
    lists:duplicate(max(PadCnt, 0), PadChar) ++ Str.

%% A word with only the tag bits set, as erl_term.h defines them.
tag_word(Term) ->
    case top_tag(Term) of
        {immed, small_int} -> 2#001111;   %% _TAG_IMMED1_SMALL
        {immed, atom}      -> 2#001011;   %% _TAG_IMMED2_ATOM
        {immed, nil}       -> 2#111011;   %% _TAG_IMMED2_NIL
        {immed, pid}       -> 2#000011;   %% _TAG_IMMED1_PID
        {immed, port}      -> 2#000111;   %% _TAG_IMMED1_PORT
        {cons, _}          -> 2#000001;   %% TAG_PRIMARY_LIST
        {boxed, _}         -> 2#000010    %% TAG_PRIMARY_BOXED
    end.

% end::classification[]

% tag::decoder[]
%%--------------------------------------------------------------------
%% Decode a real machine word's low tag bits (handy with GDB and ETP)
%%--------------------------------------------------------------------
tag_to_type(Word) ->
    case Word band 3 of                    %% primary tag, bits 1..0
        0 -> header;
        1 -> cons;
        2 -> boxed;
        3 ->
            case (Word bsr 2) band 3 of    %% immed1, bits 3..2
                0 -> pid;
                1 -> port;
                2 ->
                    case (Word bsr 4) band 3 of   %% immed2, bits 5..4
                        0 -> atom;
                        1 -> 'catch';
                        2 -> unused;
                        3 -> nil
                    end;
                3 -> smallint
            end
    end.
% end::decoder[]
