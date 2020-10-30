-module(rtmp_handshake_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/rtmp_handshake.hrl").

allowed_versions_test() ->
    ?assert(rtmp_handshake:decode_c0(<<3>>) =:= {ok, {#c0{version = 3}, <<>>}}).
