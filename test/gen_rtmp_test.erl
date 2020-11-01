-module(gen_rtmp_test).

-include_lib("eunit/include/eunit.hrl").

initializes_with_correct_version_test() ->
    {ok, Pid} = gen_rtmp:start_link(),
    {ok, [S0, S1, <<>>]} = gen_rtmp:data(Pid, <<3>>),
    ?assert(is_binary(S0)),
    ?assert(is_binary(S1)).
    
