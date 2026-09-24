-module(json_tokens_tests).
-export([run/0]).

run() ->
  Numbers = [{"0",0.0}, {"-0",-0.0}, {"0.5",0.5}, {"-0.5",-0.5},
    {"1e3",1.0e3}, {"1E+3",1.0e3}, {"1E-3",1.0e-3},
    {"1.0e3",1.0e3}, {"-12",-12.0}, {"10.25e-2",0.1025}],
  [begin {ok,[{number,1,Expected}],1}=json_tokens:string(Input) end || {Input,Expected} <- Numbers],
  Strings = [{"\"\"",[]}, {"\"\\u00AF\"",[175]}, {"\"\\u00af\"",[175]},
    {"\"\\n\\t\\b\\f\\r\"",[10,9,8,12,13]},
    {"\"\\\"\\\\\\/\"",[$",$\\,$/]}, {[$",955,$"],[955]}],
  [begin {ok,[{string,1,Expected}],1}=json_tokens:string(Input) end || {Input,Expected} <- Strings],
  [begin {error,_,_}=json_tokens:string([$",C,$"]) end || C <- lists:seq(0,31)],
  [begin {error,_,_}=json_tokens:string(Input) end || Input <- ["\"\\x\"", "\"\\u12XZ\"", "\"unterminated"]],
  [begin case json_tokens:string(Input) of
     {error,_,_} -> ok;
     {ok,Tokens,_} -> {error,_} = yecc_json_parser:parse(Tokens)
   end end || Input <- ["01", "-01", "1.", ".5", "1e", "+1"]],
  {ok,[{string,1,[16#D834,16#DD1E]}],1}=json_tokens:string("\"\\uD834\\uDD1E\""),
  {ok,[{number,1,Rounded}],1}=json_tokens:string("9007199254740993"),
  9007199254740992 = trunc(Rounded),
  ok = try json_tokens:string("1e9999"), unexpected_success catch error:badarg -> ok end,
  {ok, Fixture}=file:read_file("code/compiler_chapter/src/test.json"),
  {ok,Tokens,_}=json_tokens:string(unicode:characters_to_list(Fixture)),
  {ok,Json}=yecc_json_parser:parse(Tokens),
  #{"no" := 1.0,"name" := "Jack \"Bee\" Nimble", "format" := #{"widths" := {1920.0,1600.0},"height" := -1080.0,"unicode" := "/"}} = Json,
  io:format("60 JSON cases and the published fixture passed.~n"),
  ok.
