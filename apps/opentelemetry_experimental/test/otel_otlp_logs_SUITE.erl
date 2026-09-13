-module(otel_otlp_logs_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-define(TRACE_ID, <<16#abcdef0123456789abcdef0123456789:128>>).
-define(SPAN_ID, <<16#a1b2c3d4e5f60718:64>>).
-define(HEX_TRACE_ID, <<"abcdef0123456789abcdef0123456789">>).
-define(HEX_SPAN_ID, <<"a1b2c3d4e5f60718">>).

all() ->
    [hex_binary_ids,
     uppercase_hex_binary_ids,
     charlist_ids,
     raw_binary_ids,
     ids_without_trace_flags,
     non_hex_ids_dropped,
     wrong_size_ids_dropped,
     partially_invalid_ids_dropped,
     no_span_ctx_metadata,
     non_chardata_list_ids_dropped,
     non_hex_trace_flags_default_to_zero].

%% ids set by `otel_span:hex_span_ctx/1' are lowercase hex binaries and
%% must be decoded to the raw bytes the OTLP LogRecord expects
hex_binary_ids(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => ?HEX_TRACE_ID,
                                    otel_span_id => ?HEX_SPAN_ID,
                                    otel_trace_flags => <<"01">>}),
    ?assertMatch(#{trace_id := ?TRACE_ID,
                   span_id := ?SPAN_ID,
                   trace_flags := 1}, LogRecord).

uppercase_hex_binary_ids(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => <<"ABCDEF0123456789ABCDEF0123456789">>,
                                    otel_span_id => <<"A1B2C3D4E5F60718">>,
                                    otel_trace_flags => <<"01">>}),
    ?assertMatch(#{trace_id := ?TRACE_ID,
                   span_id := ?SPAN_ID,
                   trace_flags := 1}, LogRecord).

%% older versions of `otel_span:hex_span_ctx/1' returned charlists
charlist_ids(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => "abcdef0123456789abcdef0123456789",
                                    otel_span_id => "a1b2c3d4e5f60718",
                                    otel_trace_flags => <<"01">>}),
    ?assertMatch(#{trace_id := ?TRACE_ID,
                   span_id := ?SPAN_ID,
                   trace_flags := 1}, LogRecord).

%% raw ids pass through unchanged
raw_binary_ids(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => ?TRACE_ID,
                                    otel_span_id => ?SPAN_ID,
                                    otel_trace_flags => <<"00">>}),
    ?assertMatch(#{trace_id := ?TRACE_ID,
                   span_id := ?SPAN_ID,
                   trace_flags := 0}, LogRecord).

ids_without_trace_flags(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => ?HEX_TRACE_ID,
                                    otel_span_id => ?HEX_SPAN_ID}),
    ?assertMatch(#{trace_id := ?TRACE_ID,
                   span_id := ?SPAN_ID}, LogRecord),
    ?assertNot(maps:is_key(trace_flags, LogRecord)).

%% ids that can't be decoded must be dropped, along with the trace
%% flags, instead of being exported as garbage
non_hex_ids_dropped(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => binary:copy(<<"zz">>, 16),
                                    otel_span_id => binary:copy(<<"zz">>, 8),
                                    otel_trace_flags => <<"01">>}),
    assert_no_span_ctx(LogRecord).

wrong_size_ids_dropped(_Config) ->
    %% odd number of hex digits
    LogRecord = export_log_record(#{otel_trace_id => <<"abcdef0123456789abcdef012345678">>,
                                    otel_span_id => <<"a1b2c3d4e5f6071">>,
                                    otel_trace_flags => <<"01">>}),
    assert_no_span_ctx(LogRecord).

partially_invalid_ids_dropped(_Config) ->
    %% a valid trace id with an invalid span id must drop both ids
    LogRecord = export_log_record(#{otel_trace_id => ?HEX_TRACE_ID,
                                    otel_span_id => <<"not a span id">>,
                                    otel_trace_flags => <<"01">>}),
    assert_no_span_ctx(LogRecord).

no_span_ctx_metadata(_Config) ->
    LogRecord = export_log_record(#{}),
    assert_no_span_ctx(LogRecord),
    %% the rest of the LogRecord is built as usual
    ?assertMatch(#{body := _,
                   severity_number := 'SEVERITY_NUMBER_INFO',
                   time_unix_nano := _}, LogRecord).

%%

export_log_record(SpanCtxMetadata) ->
    Metadata = SpanCtxMetadata#{time => erlang:system_time(microsecond)},
    LogEvent = #{level => info,
                 msg => {string, "hello"},
                 meta => Metadata},
    Scope = #instrumentation_scope{name = <<"test_scope">>,
                                   version = <<"1.0.0">>},
    Resource = otel_resource:create([]),
    #{resource_logs :=
          [#{scope_logs :=
                 [#{log_records := [LogRecord]}]}]} =
        otel_otlp_logs:to_proto(#{Scope => [LogEvent]}, Resource, #{}),
    LogRecord.

assert_no_span_ctx(LogRecord) ->
    ?assertNot(maps:is_key(trace_id, LogRecord)),
    ?assertNot(maps:is_key(span_id, LogRecord)),
    ?assertNot(maps:is_key(trace_flags, LogRecord)).

non_chardata_list_ids_dropped(_Config) ->
    LogRecord = export_log_record(#{otel_trace_id => [an_atom, "abc"],
                                    otel_span_id => "6d4d6b6532a1c2d3"}),
    ?assertNot(maps:is_key(trace_id, LogRecord)),
    ?assertNot(maps:is_key(span_id, LogRecord)),
    ok.

non_hex_trace_flags_default_to_zero(_Config) ->
    %% present-but-garbage flags must not raise from binary_to_integer/2.
    LogRecord = export_log_record(#{otel_trace_id => <<"00112233445566778899aabbccddeeff">>,
                                    otel_span_id => <<"0011223344556677">>,
                                    otel_trace_flags => <<"zz">>}),
    ?assertEqual(<<16#00112233445566778899aabbccddeeff:128>>, maps:get(trace_id, LogRecord)),
    ?assertEqual(<<16#0011223344556677:64>>, maps:get(span_id, LogRecord)),
    ?assertEqual(0, maps:get(trace_flags, LogRecord)),
    ok.
