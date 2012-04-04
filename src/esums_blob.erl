-module(esums_blob).

-export([
    calc/1
]).

calc(Data) ->
    esums:final(esums:new(), Data).
