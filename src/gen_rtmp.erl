-module(gen_rtmp).

-behaviour(gen_statem).

-export([start_link/0, stop/1, data/2]).
-export([callback_mode/0, init/1]).
-export([uninitialized/3, waiting_c1/3, waiting_c2/3, awaiting_chunk/3]).

-include("rtmp_handshake.hrl").

-record(state, {incomplete_data :: binary()}).

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
    {ok, State, #state{incomplete_data = <<>>}}.

%% State Callbacks
uninitialized({call, From}, Bin, State) ->
    case rtmp_handshake:decode_c0(Bin) of
        {ok, {C0, C1Bin}} ->
            S0 = rtmp_handshake:encode_s0(C0#c0.version),
            S1 = rtmp_handshake:encode_s1(0, crypto:strong_rand_bytes(1528)),
            {NextState, MaybeS2, Additional} = maybe_c1(State#state.incomplete_data, C1Bin),
            {next_state, NextState, State#state{incomplete_data = Additional}, [
                {reply, From, {ok, [S0, S1, MaybeS2]}}
            ]};
        {error, {unknown_version, Vsn}} = Error ->
            {stop_and_reply, unknown_version, [{reply, From, Error}]}
    end.

waiting_c1({call, From}, Bin, State) ->
    IncompleteData = State#state.incomplete_data,
    {C1, Rest} = rtmp_handshake:decode_c1(<<IncompleteData/binary, Bin/binary>>),
    S2 = rtmp_handshake:encode_s2(0, C1#c1.time, C1#c1.random_bytes),
    {next_state, waiting_c2, State#state{incomplete_data = Rest}, [{reply, From, S2}]}.

waiting_c2({call, From}, Bin, State) ->
    IncompleteData = State#state.incomplete_data,
    Full = <<IncompleteData/binary, Bin/binary>>,
    case byte_size(Full) >= 1532 of
        true ->
            % TODO: Check that the echo is correct.
            {_C2, Rest} = rtmp_handshake:decode_c2(Full),
            {next_state, awaiting_chunk, State#state{incomplete_data = Rest}, [{reply, From, []}]};
        false ->
            {keep_state, State#state{incomplete_data = Full}, [{reply, From, []}]}
    end.

awaiting_chunk({call, _From}, Bin, State) ->
    {keep_state, State}.

%% uninitialized({call, From}, Bin, Data) ->
%%     {C0, C1Bin} = rtmp_handshake:decode_c0(Bin),
%%     S0 = rtmp_handshake:encode_s0(C0#c0.version),
%%     S1 = rtmp_handshake:encode_s1(0, crypto:strong_rand_bytes(50)),
%%     {next_state, version_sent, Data, [{reply, From, {S0, S1}}]}.

%% version_sent({call, From}, Bin, Data) ->
%%     C1 = rtmp_handshake:decode_c1(Bin),
%%     S2 = "something",
%%     {next_state, ack_sent, Data, [{reply, From, S2}]}.

%% ack_sent({call, From}, Bin, Data) ->
%%     C2 = rtmp_handshake:decode_c2(Bin),
%%     S3 = "foo",
%%     {next_state, handshake_done, Data, [{reply, From, S3}]}.

%% Handle Events

%% Private
-spec maybe_c1(binary(), binary()) -> {atom(), binary(), binary()}.
maybe_c1(Incomplete, Bin) when byte_size(<<Incomplete/binary, Bin/binary>>) < 1536 ->
    {waiting_c1, <<>>, <<Incomplete/binary, Bin/binary>>};
maybe_c1(Incomplete, Bin) ->
    {C1, Rest} = rtmp_handshake:decode_c1(<<Incomplete/binary, Bin/binary>>),
    S2 = rtmp_handshake:encode_s2(0, C1#c1.time, C1#c1.random_bytes),
    {waiting_c2, S2, Rest}.
