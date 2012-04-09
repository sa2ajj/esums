-module(esums_file).

-export([
    calc/1,
    open/1,
    write/2,
    complete/2,
    complete/1,
    info/1
]).

-define(BLOCK_SIZE, 128*1024).

-record(esums_file, {
    name :: binary(),
    mtime :: integer(),
    fd :: term(),
    completed = false :: boolean(),
    sums :: term()
}).

calc(FileName) ->
    {ok, Device} = file:open(FileName, [read, raw, binary]),
    Result = read_all(Device, ?BLOCK_SIZE, esums:new()),
    file:close(Device),
    Result.

read_all(Device, BlockSize, Context) ->
    case file:read(Device, BlockSize) of
        {ok, Data} ->
            read_all(Device, BlockSize, esums:update(Context, Data));

        eof ->
            esums:final(Context)
    end.

open(Name) when is_binary(Name) ->
    case file:open(Name, [write, raw, delayed_write]) of
        {ok, FD} ->
            #esums_file{
                name = Name,
                mtime = esums_helpers:utc(),
                fd = FD,
                completed = false,
                sums = esums:new()
            };

        Other ->
            Other
    end;
open(Name) when is_list(Name) ->
    open(list_to_binary(Name)).

write(#esums_file{fd=FD, completed=false, sums=Sums}=State, Buffer) when is_binary(Buffer) ->
    case file:write(FD, Buffer) of
        ok ->
            State#esums_file{
                mtime = esums_helpers:utc(),
                sums = esums:update(Sums, Buffer)
            };

        Other ->
            Other
    end.

complete(State, Buffer) ->
    complete(write(State, Buffer)).

complete(#esums_file{fd=FD, completed=false, sums=Sums}=State) ->
    ok = file:close(FD),
    State#esums_file{mtime = esums_helpers:utc(), completed=true, sums=esums:final(Sums)}.

info(#esums_file{name=Name, mtime=MTime, completed=true, sums=Sums}) ->
    {Name, MTime, Sums}.
