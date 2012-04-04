-module(esums_file).

-export([
    calc/1
]).

-define(BLOCK_SIZE, 128*1024).

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
