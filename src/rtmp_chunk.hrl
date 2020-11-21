-record(type0, {
    timestamp :: non_neg_integer(),
    message_length :: non_neg_integer(),
    message_type_id :: non_neg_integer(),
    message_stream_id :: non_neg_integer()
}).

-record(type1, {
    timestamp_delta :: non_neg_integer(),
    message_length :: non_neg_integer(),
    message_type_id :: non_neg_integer()
}).

-record(type2, {
    timestamp_delta :: non_neg_integer()
}).

-record(type3, {}).

%% Protocol Control Messages
-record(set_chunk_size, {size :: non_neg_integer()}).

-record(abort, {chunk_stream_id :: non_neg_integer()}).

-record(acknowledgement, {sequence_number :: non_neg_integer()}).

-record(window_acknowledgement_size, {window_size :: non_neg_integer()}).

-record(set_peer_bandwidth, {
    window_size :: non_neg_integer(),
    limit_type :: hard | soft | dynamic
}).
