-module(esums).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-export([
    new/0,
    update/2,
    final/2,
    final/1,
    simple/2,
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

simple(md5, Data) ->
    crypto:md5_final(crypto:md5_update(crypto:md5_init(), Data));
simple(sha1, Data) ->
    crypto:sha_final(crypto:sha_update(crypto:sha_init(), Data));
simple(sha256, Data) ->
    erlsha2:sha256_final(erlsha2:sha256_update(erlsha2:sha256_init(), Data));
simple(size, Data) ->
    byte_size(Data).

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

parse(md5, Hash) when is_binary(Hash), byte_size(Hash) == 32 ->
    {ok, [MD5], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<MD5:128/big-unsigned-integer>>;
parse(sha1, Hash) when is_binary(Hash), byte_size(Hash) == 40 ->
    {ok, [SHA1], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<SHA1:160/big-unsigned-integer>>;
parse(sha256, Hash) when is_binary(Hash), byte_size(Hash) == 64 ->
    {ok, [SHA256], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<SHA256:256/big-unsigned-integer>>.

% {{{ Helpers
format_to_bin(Format, Data) ->
    list_to_binary(io_lib:format(Format, Data)).
% }}}

% {{{ Unit tests
-ifdef(TEST).
format_hash_test_() ->
    MD5 = <<197,109,186,109,175,244,61,2,70,52,250,214,23,72,239,156>>,
    SHA1 = <<180,33,100,219,103,33,14,21,200,207,152,52,86,32,11,199,32,223,63,120>>,
    SHA256 = <<16,187,204,5,54,185,28,29,232,181,196,161,34,255,188,225,62,79,84,114,204,41,49,170,47,212,146,244,190,197,171,244>>,
    {"format_hash simple tests", [
        ?_assertEqual(MD5, parse(md5, format(md5, MD5))),
        ?_assertEqual(SHA1, parse(sha1, format(sha1, SHA1))),
        ?_assertEqual(SHA256, parse(sha256, format(sha256, SHA256)))
    ]}.
-endif.
% }}}

% vim:sw=4:ts=4:et:ai
