%%%-------------------------------------------------------------------
%%%  show.erl  – minimal BEAM tag sniffing in pure Erlang
%%%-------------------------------------------------------------------
-module(show).

-export([top_tag/1,               %% → {primary,sub}
         tag/1, hex_tag/1,        %% → zero-padded string of low tag bits
         tag_to_type/1]).         %% → header|cons|boxed|pid|…

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
%% High-level tag classifier
%%--------------------------------------------------------------------
top_tag(Term) when is_integer(Term) ->
    {Min,Max} = small_limits(),
    if  Term >= Min, Term =< Max ->
            {immed, small_int};          %% primary 11, sub 11
        true ->
            {boxed, bignum_header}
    end;
top_tag(Term) when is_atom(Term)     -> {immed, atom};
top_tag(true)                        -> {immed, bool};
top_tag(false)                       -> {immed, bool};
top_tag([])                          -> {immed, nil};
top_tag(Term) when is_pid(Term)      -> {immed, pid};
top_tag(Term) when is_port(Term)     -> {immed, port};
top_tag(Term) when is_reference(Term)-> {immed, ref};

top_tag([_|_])                       -> {cons_cell, list_ptr};   %% primary 01
top_tag({_})                         -> {tuple_boxed, boxed_ptr};
top_tag(Term) when is_float(Term)    -> {float_boxed, boxed_ptr};
top_tag(Term) when is_binary(Term)   -> {bin_boxed,   boxed_ptr};
top_tag(Term) when is_map(Term)      -> {map_boxed,   boxed_ptr};
top_tag(Term) when is_function(Term) -> {fun_boxed,   boxed_ptr};

top_tag(_)                           -> {unknown, needs_native}.

%%--------------------------------------------------------------------
%% Pretty-print helpers (zero-padded)
%%--------------------------------------------------------------------
tag(Term) ->
    pad_left(integer_to_list(tag_word(Term), 2), word_bits(), $0).

hex_tag(Term) ->
    pad_left(string:uppercase(integer_to_list(tag_word(Term), 16)),
             hex_digits(), $0).

pad_left(Str, Width, PadChar) ->
    PadCnt = Width - length(Str),
    lists:duplicate(max(PadCnt,0), PadChar) ++ Str.

%% minimal word with just the interesting low bits set ---------------
tag_word(Term) ->
    case top_tag(Term) of
        {immed, small_int} -> 16#3;      %% …0011
        {immed, atom}      -> 16#F;      %% …1111
        {immed, bool}      -> 16#B;      %% …1011  (same as small int tag,
                                         %%         VM distinguishes by range)
        {immed, nil}       -> 16#1F;     %% …11111
        {immed, pid}       -> 16#7;      %% …0111
        {immed, port}      -> 16#B;      %% …1011
        {cons_cell,_}      -> 16#1;      %% …0001
        {boxed,_}          -> 16#2;      %% …0010
        _                  -> 0
    end.

%%--------------------------------------------------------------------
%% Decode a *real* machine word’s low tag bits (handy for GDB/ETP)
%%--------------------------------------------------------------------
tag_to_type(Word) ->
    case Word band 3 of          %% primary tag = two LSBs
        0 -> header;
        1 -> cons;
        2 -> boxed;
        3 ->
            case (Word bsr 2) band 3 of    %% immed-1 secondary tag
                0 -> pid;
                1 -> port;
                2 ->
                    case (Word bsr 4) band 3 of
                        0 -> atom;
                        1 -> 'catch';     %% quoted – ‘catch’ is a keyword
                        2 -> unused;
                        3 -> nil
                    end;
                3 -> smallint
            end
    end.
