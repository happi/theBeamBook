-module(record_walk).
-export([compare/0]).
-record(old_person, {name, age}).
-record #person{name, age}.

compare() ->
    Old = #old_person{name = alice, age = 30},
    Native = #person{name = alice, age = 30},
    {is_tuple(Old), is_tuple(Native), is_record(Native)}.
