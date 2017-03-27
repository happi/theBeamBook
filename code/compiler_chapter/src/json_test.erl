-module(json_test).
-compile({parse_transform, json_parser}).
-export([test/1]).

test(V) ->
    <<{{
      "name"  : "Jack (\"Bee\") Nimble",
      "format": {
                  "type"      : "rect",
                  "widths"     : [1920,1600],
                  "height"    : (-1080),
                  "interlace" : false,
                  "frame rate": V
                }
     }}>>.
