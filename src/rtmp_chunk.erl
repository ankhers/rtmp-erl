-module(rtmp_chunk).

-export([decode/1]).

decode(Bin) ->
    {Fmt, CsId, Rest0} = decode_basic_header(Bin),
    {TS, Len, Id, MsgSId, _Rest1} = decode_message_header(Fmt, Rest0),
    io:format("Format: ~p, Chunk Stream ID: ~p~n", [Fmt, CsId]),
    io:format("Timestamp: ~p, Length: ~p, Id: ~p, Stream Id: ~p~n", [TS, Len, Id, MsgSId]),
    <<>>.

%% decode_header(Bin) ->
%%     {Basic, Rest} = decode_basic_header(Bin),
%%     {Message, Rest1} = decode_message_header(Rest),
%%     {Timestamp, Rest2} = decode_extended_timestamp(Rest1),
%%     {Chunk, Rest3} = decode_chunk_data(Rest2),
%%     {Basic, Message, Timestamp, Chunk}.

decode_basic_header(<<Fmt:2, 0:6, CsId, Rest/binary>>) ->
    {Fmt, CsId - 64, Rest};
decode_basic_header(<<Fmt:2, 1:6, CsId:16, Rest/binary>>) ->
    {Fmt, CsId - 64, Rest};
decode_basic_header(<<Fmt:2, CsId:6, Rest/binary>>) ->
    {Fmt, CsId, Rest}.

decode_message_header(0, <<Timestamp0:24, MsgLen:24, MsgTypeId, MsgStreamId:32, Rest0/binary>>) ->
    {Timestamp, Rest} = maybe_extended_timestamp(Timestamp0, Rest0),
    {Timestamp, MsgLen, MsgTypeId, MsgStreamId, Rest};
decode_message_header(1, <<TimestampDelta0:24, MsgLen:24, MsgTypeId, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {TimestampDelta, MsgLen, MsgTypeId, -1, Rest};
decode_message_header(2, <<TimestampDelta0:24, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {TimestampDelta, -1, -1, -1, Rest};
decode_message_header(3, <<>>) ->
    <<>>.

maybe_extended_timestamp(Timestamp, <<FullTimestamp:32, Rest>>) when Timestamp =:= 16#ffffff ->
    {FullTimestamp, Rest};
maybe_extended_timestamp(Timestamp, Rest) ->
    {Timestamp, Rest}.
