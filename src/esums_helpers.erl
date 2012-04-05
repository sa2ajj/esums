-module(esums_helpers).

-export([
    utc/0,
    posix_time/1
]).

% I'm not sure if this function belongs here
utc() ->
    posix_time(calendar:universal_time()).

% copied from erl_tar, this won't be needed if a newer version of erlang is
% used (where file:read_file_info/2 is available).
posix_time(Time) ->
    EpochStart = {{1970,1,1},{0,0,0}},
    {Days,{Hour,Min,Sec}} = calendar:time_difference(EpochStart, Time),
    86400*Days + 3600*Hour + 60*Min + Sec.
