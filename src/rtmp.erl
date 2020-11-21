-module(rtmp).

-behaviour(gen_statem).

-export([start_link/0, stop/1, data/2]).
-export([callback_mode/0, init/1]).
-export([uninitialized/3, waiting_c1/3, waiting_c2/3, awaiting_chunk/3]).

-include("rtmp_chunk.hrl").
-include("rtmp_handshake.hrl").

-record(state, {incomplete_data :: binary(), chunk_size :: non_neg_integer()}).

%% The client MUST wait until S1 has been received before sending C2.
%% The client MUST wait until S2 has been received before sending any other data.

%% The server MUST wait until C0 has been received before sending S0 and S1, and MAY wait until after C1 as well.
%% The server MUST wait until C1 has been received before sending S2.
%% The server MUST wait until C2 has been received before sending any other data.

%% API
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_statem:stop(Pid).

data(Pid, Bin) ->
    gen_statem:call(Pid, Bin).

%% Callbacks
callback_mode() -> state_functions.

init([]) ->
    State = uninitialized,
    {ok, State, #state{incomplete_data = <<>>, chunk_size = 128}}.

%% State Callbacks
uninitialized({call, From}, Bin, State) ->
    case rtmp_handshake:decode_c0(Bin) of
        {ok, C0, C1Bin} ->
            S0 = rtmp_handshake:encode_s0(C0#c0.version),
            S1 = rtmp_handshake:encode_s1(0, crypto:strong_rand_bytes(1528)),
            case maybe_c1(State#state.incomplete_data, C1Bin) of
                {error, Reason} = Error ->
                    {stop_and_reply, Reason, [{reply, From, Error}]};
                {ok, NextState, MaybeS2, Additional} ->
                    {next_state, NextState, State#state{incomplete_data = Additional}, [
                        {reply, From, {ok, [S0, S1, MaybeS2]}}
                    ]}
            end;
        {error, unknown_version, _Vsn} = Error ->
            {stop_and_reply, unknown_version, [{reply, From, Error}]}
    end.

waiting_c1({call, From}, Bin, State) ->
    IncompleteData = State#state.incomplete_data,
    Full = <<IncompleteData/binary, Bin/binary>>,
    case rtmp_handshake:decode_c1(Full) of
        {ok, C1, Rest} ->
            S2 = rtmp_handshake:encode_s2(0, C1#c1.time, C1#c1.random_bytes),
            {next_state, waiting_c2, State#state{incomplete_data = Rest}, [{reply, From, {ok, S2}}]};
        {error, insufficient_data} ->
            {keep_state, State#state{incomplete_data = Full}, [{reply, From, ok}]}
    end.

waiting_c2({call, From}, Bin, State) ->
    IncompleteData = State#state.incomplete_data,
    Full = <<IncompleteData/binary, Bin/binary>>,
    case rtmp_handshake:decode_c2(Full) of
        {ok, _C2, Rest} ->
            {next_state, awaiting_chunk, State#state{incomplete_data = Rest}, [{reply, From, ok}]};
        {error, insufficient_data} ->
            {keep_state, State#state{incomplete_data = Full}, [{reply, From, ok}]}
    end.

awaiting_chunk({call, _From}, Bin, State) ->
    case rtmp_chunk:decode(Bin) of
        {#set_chunk_size{size = ChunkSize}, Rest} ->
            {keep_state, State#state{incomplete_data = Rest, chunk_size = ChunkSize}}
    end.

%% Handle Events

%% Private
-spec maybe_c1(binary(), binary()) -> {ok, atom(), binary(), binary()} | {error, atom()}.
maybe_c1(Incomplete, Bin) ->
    Full = <<Incomplete/binary, Bin/binary>>,
    case rtmp_handshake:decode_c1(Full) of
        {ok, C1, Rest} ->
            S2 = rtmp_handshake:encode_s2(0, C1#c1.time, C1#c1.random_bytes),
            {ok, waiting_c2, S2, Rest};
        {error, insufficient_data} ->
            {ok, waiting_c1, <<>>, <<Incomplete/binary, Bin/binary>>};
        {error, bad_format} = Error ->
            Error
    end.
