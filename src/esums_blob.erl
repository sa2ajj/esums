-module(esums_blob).
-behaviour(esums_pair).

-export([
    calc/1,
    open/1,
    write/2,
    complete/2,
    complete/1,
    info/1,
    name/1,
    data/1,
    size/1,
    sums/1
]).

-record(esums_blob, {
    name :: binary(),
    mtime :: integer(),
    data = <<>> :: binary(),
    completed = false :: boolean(),
    sums :: term()
}).

calc(Data) ->
    esums:final(esums:new(), Data).

open(Name) when is_binary(Name) ->
    #esums_blob{
        name = Name,
        mtime = esums_helpers:utc(),
        data = <<>>,
        completed = false,
        sums = esums:new()
    }.

write(#esums_blob{data=Data, completed=false, sums=Sums}=State, Buffer) when is_binary(Buffer) ->
    State#esums_blob{
        mtime = esums_helpers:utc(),
        data = <<Data/binary, Buffer/binary>>,
        sums = esums:update(Sums, Buffer)
    }.

complete(State, Buffer) ->
    complete(write(State, Buffer)).

complete(#esums_blob{completed=false, sums=Sums}=State) ->
    State#esums_blob{mtime = esums_helpers:utc(), completed=true, sums=esums:final(Sums)}.

info(#esums_blob{name=Name, mtime=MTime, completed=true, sums=Sums}) ->
    {Name, MTime, Sums}.

name(#esums_blob{name=Name}) ->
    Name.

data(#esums_blob{data=Data}) ->
    Data.

size(#esums_blob{data=Data}) ->
    byte_size(Data).

sums(#esums_blob{completed=true, sums=Sums}) ->
    Sums.
