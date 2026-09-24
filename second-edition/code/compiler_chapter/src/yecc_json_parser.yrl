
Nonterminals value values object array pair pairs.

Terminals number string true false null '[' ']' '{' '}' ',' ':'.

Rootsymbol value.

value -> object  :  '$1'.
value -> array   :  '$1'.
value -> number  :  get_val('$1').
value -> string  :  get_val('$1').
value -> 'true'  :  get_val('$1').
value -> 'null'  :  get_val('$1').
value -> 'false' :  get_val('$1').

object -> '{' '}' : #{}.
object -> '{' pairs '}' : '$2'.

pairs -> pair : '$1'.
pairs -> pair ',' pairs : maps:merge('$1', '$3').

pair -> string ':' value : #{ get_val('$1') => '$3' }.

array -> '[' ']' : {}.
array -> '[' values ']' : list_to_tuple('$2').

values -> value : [ '$1' ].
values -> value ',' values : [ '$1' | '$3' ].



Erlang code.

get_val({_,_,Val}) -> Val;
get_val({Val, _}) -> Val.



