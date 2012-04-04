-module(esums).

-export([
    new/0,
    update/2,
    final/2,
    final/1,
    format/1,
    format/2,
    parse/2
]).

-record(esums, {
    size :: integer(),
    md5 :: term(),
    sha1 :: term(),
    sha256 :: term()
}).

-define(MD5_FORMAT, "~32.16.0b").
-define(SHA1_FORMAT, "~40.16.0b").
-define(SHA256_FORMAT, "~64.16.0b").

new() ->
    #esums{size=0, md5=crypto:md5_init(), sha1=crypto:sha_init(), sha256=erlsha2:sha256_init()}.

update(#esums{size=Size, md5=MD5, sha1=SHA1, sha256=SHA256}, Data) when is_binary(Data) ->
    #esums{size=Size+byte_size(Data),
           md5=crypto:md5_update(MD5, Data),
           sha1=crypto:sha_update(SHA1, Data),
           sha256=erlsha2:sha256_update(SHA256, Data)}.

final(#esums{}=Context, Data) ->
    final(update(Context, Data)).

final(#esums{size=Size, md5=MD5, sha1=SHA1, sha256=SHA256}) -> [
    {md5, crypto:md5_final(MD5)},
    {sha1, crypto:sha_final(SHA1)},
    {sha256, erlsha2:sha256_final(SHA256)},
    {size, Size}
].

format({Kind, Hash}) ->
    format(Kind, Hash).

format(md5, Hash) ->
    <<MD5:128/big-unsigned-integer>> = Hash,
    format_to_bin(?MD5_FORMAT, [MD5]);
format(sha1, Hash) ->
    <<SHA1:160/big-unsigned-integer>> = Hash,
    format_to_bin(?SHA1_FORMAT, [SHA1]);
format(sha256, Hash) ->
    <<SHA256:256/big-unsigned-integer>> = Hash,
    format_to_bin(?SHA256_FORMAT, [SHA256]);
format(size, Hash) ->
    format_to_bin("~b", [Hash]).

parse(Hash, md5) when is_binary(Hash), byte_size(Hash) == 32 ->
    {ok, [MD5], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<MD5:128/big-unsigned-integer>>;
parse(Hash, sha1) when is_binary(Hash), byte_size(Hash) == 40 ->
    {ok, [SHA1], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<SHA1:160/big-unsigned-integer>>;
parse(Hash, sha256) when is_binary(Hash), byte_size(Hash) == 64 ->
    {ok, [SHA256], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<SHA256:256/big-unsigned-integer>>.

% {{{ Helpers
format_to_bin(Format, Data) ->
    list_to_binary(io_lib:format(Format, Data)).
% }}}
