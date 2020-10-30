-module(rtmp_handshake).

-export([decode_c0/1, decode_c1/1, decode_c2/1]).
-export([encode_s0/1, encode_s1/2, encode_s2/3]).

-include("rtmp_handshake.hrl").

-spec decode_c0(binary()) -> {#c0{}, binary()}.
decode_c0(<<3, Rest/binary>>) ->
    {#c0{version = 3}, Rest}.

-spec encode_s0(integer()) -> binary().
encode_s0(Version) ->
    <<Version:8>>.

-spec decode_c1(binary()) -> {#c1{}, binary()}.
decode_c1(<<Time:32, 0:32, RandomBytes:1528/binary, Rest/binary>>) ->
    {#c1{time = Time, random_bytes = RandomBytes}, Rest}.

-spec encode_s1(integer(), binary()) -> binary().
encode_s1(Time, RandomBytes) ->
    <<Time:32, 0:32, RandomBytes/binary>>.

-spec decode_c2(binary()) -> {#c2{}, binary()}.
decode_c2(<<Time:32, Time2:32, Echo:1528/binary, Rest/binary>>) ->
    {#c2{time = Time, time2 = Time2, random_echo = Echo}, Rest}.

-spec encode_s2(integer(), integer(), binary()) -> binary().
encode_s2(Time, Time2, RandomEcho) ->
    <<Time:32, Time2:32, RandomEcho/binary>>.
