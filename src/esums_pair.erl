-module(esums_pair).

-export([
    open/2,
    open/3,
    write/2,
    complete/2,
    complete/1,
    info/1,
    info/2
]).

-record(esums_pair, {
    zz :: term(),
    module :: atom(),
    plain :: 'not_used' | term(),
    sums :: 'not_used' | term(),
    gzipped :: term()
}).

open(Name, Module) ->
    open(Name, Module, true).

open(Name, Module, Both) when is_binary(Name), is_atom(Module), is_boolean(Both) ->
    ZZ = zlib:open(),
    ok = zlib:deflateInit(ZZ, default, deflated, 31, 8, default),
    {Plain, Sums} = if
        Both ->
            {Module:open(Name), 'not_used'};

        true ->
            {'not_used', esums:new()}
    end,
    #esums_pair{zz=ZZ,
                module=Module,
                plain=Plain,
                sums=Sums,
                gzipped=Module:new(<<Name/binary, ".gz">>)}.

write(#esums_pair{zz=ZZ, module=Module, plain=Plain, sums=Sums, gzipped=GZipped}=State, Buffer) when is_binary(Buffer) ->
    Compressed = iolist_to_binary(zlib:deflate(ZZ, Buffer)),
    {NewPlain, NewSums} = write(Module, Plain, Sums, Buffer),
    State#esums_pair{
        plain=NewPlain,
        sums=NewSums,
        gzipped=Module:write(GZipped, Compressed)
    }.

write(_, 'not_used', Sums, Buffer) ->
    {'not_used', esums:update(Sums, Buffer)};
write(Module, Plain, 'not_used', Buffer) ->
    {Module:write(Plain, Buffer), 'not_used'}.

complete(State, Buffer) ->
    complete(write(State, Buffer)).

complete(#esums_pair{zz=ZZ, module=Module, plain=Plain, sums=Sums, gzipped=GZipped}=State) ->
    Last = iolist_to_binary(zlib:deflate(ZZ, [], finish)),
    ok = zlib:deflateEnd(ZZ),
    zlib:close(ZZ),
    {NewPlain, NewSums} = complete(Module, Plain, Sums),
    State#esums_pair{
        plain=NewPlain,
        sums=NewSums,
        gzipped=Module:complete(GZipped, Last)
    }.

complete(_, 'not_used', Sums) ->
    {'not_used', esums:final(Sums)};
complete(Module, Plain, 'not_used') ->
    {Module:complete(Plain), 'not_used'}.

info(State) ->
    {info(State, plain), info(State, gzipped)}.

info(#esums_pair{plain='not_used', sums=Sums}, plain) ->
    {no_name, Sums};
info(#esums_pair{module=Module, plain=Plain, sums='not_used'}, plain) ->
    Module:info(Plain);
info(#esums_pair{module=Module, gzipped=GZipped}, gzipped) ->
     Module:info(GZipped).
