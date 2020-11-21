-module(rtmp_chunk).

-export([decode/1]).

-include("rtmp_chunk.hrl").

-type header_type() :: #type0{} | #type1{} | #type2{} | #type3{}.

-type control_message() ::
    #set_chunk_size{}
    | #abort{}
    | #acknowledgement{}
    | #window_acknowledgement_size{}
    | #set_peer_bandwidth{}.

-spec decode(binary()) -> {control_message(), binary()} | incomplete.
decode(<<>>) ->
    incomplete;
decode(Bin) ->
    {Fmt, CsId, Rest0} = decode_basic_header(Bin),
    {Type, Rest1} = decode_message_header(Fmt, Rest0),
    decode_chunk_data(CsId, Type, Rest1).

%% decode_header(Bin) ->
%%     {Basic, Rest} = decode_basic_header(Bin),
%%     {Message, Rest1} = decode_message_header(Rest),
%%     {Timestamp, Rest2} = decode_extended_timestamp(Rest1),
%%     {Chunk, Rest3} = decode_chunk_data(Rest2),
%%     {Basic, Message, Timestamp, Chunk}.

-spec decode_basic_header(binary()) -> {format(), chunk_stream_id(), binary()}.
decode_basic_header(<<Fmt:2, 0:6, CsId, Rest/binary>>) ->
    {Fmt, CsId - 64, Rest};
decode_basic_header(<<Fmt:2, 1:6, CsId:16, Rest/binary>>) ->
    {Fmt, CsId - 64, Rest};
decode_basic_header(<<Fmt:2, CsId:6, Rest/binary>>) ->
    {Fmt, CsId, Rest}.

-spec decode_message_header(format(), binary()) -> {header_type(), binary()}.
decode_message_header(0, <<Timestamp0:24, MsgLen:24, MsgTypeId, MsgStreamId:32, Rest0/binary>>) ->
    {Timestamp, Rest} = maybe_extended_timestamp(Timestamp0, Rest0),
    {#type0{
            timestamp = Timestamp,
            message_length = MsgLen,
            message_type_id = MsgTypeId,
            message_stream_id = MsgStreamId
        },
        Rest};
decode_message_header(1, <<TimestampDelta0:24, MsgLen:24, MsgTypeId, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {#type1{timestamp_delta = TimestampDelta, message_length = MsgLen, message_type_id = MsgTypeId},
        Rest};
decode_message_header(2, <<TimestampDelta0:24, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {#type2{timestamp_delta = TimestampDelta}, Rest};
decode_message_header(3, Rest) ->
    {#type3{}, Rest}.

-spec maybe_extended_timestamp(timestamp() | timestamp_delta(), binary()) ->
    {timestamp() | timestamp_delta(), binary()}.
maybe_extended_timestamp(Timestamp, <<FullTimestamp:32, Rest>>) when Timestamp =:= 16#ffffff ->
    {FullTimestamp, Rest};
maybe_extended_timestamp(Timestamp, Rest) ->
    {Timestamp, Rest}.

-spec decode_chunk_data(chunk_stream_id(), header_type(), binary()) ->
    {control_message(), binary()}.
decode_chunk_data(2, #type0{message_stream_id = 0, message_type_id = MsgTypeId}, Bin) ->
    decode_control_message(MsgTypeId, Bin);
decode_chunk_data(ChunkStreamId, HeaderType, Bin) ->
    io:format("Chunk Stream ID: ~p, Header Type: ~p~n", [ChunkStreamId, HeaderType]),
    io:format("Binary: ~p~n", [Bin]),
    {foo, Bin}.

decode_control_message(1, <<0:1, ChunkSize:31, Rest/binary>>) ->
    {#set_chunk_size{size = min(16#FFFFFF, ChunkSize)}, Rest};
decode_control_message(2, <<ChunkStreamId:32, Rest/binary>>) ->
    {#abort{chunk_stream_id = ChunkStreamId}, Rest};
decode_control_message(3, <<SequenceNumber:32, Rest/binary>>) ->
    {#acknowledgement{sequence_number = SequenceNumber}, Rest};
decode_control_message(5, <<AckWindowSize:32, Rest/binary>>) ->
    {#window_acknowledgement_size{window_size = AckWindowSize}, Rest};
decode_control_message(6, <<AckWindowSize:32, LimitType0, Rest/binary>>) ->
    LimitType =
        case LimitType0 of
            0 -> hard;
            1 -> soft;
            2 -> dynamic
        end,
    {#set_peer_bandwidth{window_size = AckWindowSize, limit_type = LimitType}, Rest}.
