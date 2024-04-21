-module(otel_exporter_metrics_otlp_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").
-include_lib("opentelemetry_experimental/src/otel_metric_exemplar.hrl").

all() ->
    [{group, functional}, {group, http_protobuf}, {group, http_protobuf_gzip},
     {group, grpc}, {group, grpc_gzip}].

groups() ->
    [{functional, [], [configuration]},
     {grpc, [], [verify_export]},
     {grpc_gzip, [], [verify_export]},
     {http_protobuf, [], [verify_export]},
     {http_protobuf_gzip, [], [verify_export]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) when Group =:= grpc ;
                                   Group =:= http_protobuf ->
    application:ensure_all_started(opentelemetry_exporter),
    application:ensure_all_started(opentelemetry),
    [{protocol, Group}| Config];
init_per_group(http_protobuf_gzip, Config) ->
    application:ensure_all_started(opentelemetry_exporter),
    application:ensure_all_started(opentelemetry),
    [{protocol, http_protobuf}, {compression, gzip} | Config];
init_per_group(grpc_gzip, Config) ->
    application:ensure_all_started(opentelemetry_exporter),
    application:ensure_all_started(opentelemetry),
    [{protocol, grpc}, {compression, gzip} | Config];
init_per_group(_, _) ->
    application:load(opentelemetry_exporter),
    application:ensure_all_started(opentelemetry),
    ok.

end_per_group(Group, _Config) when Group =:= grpc ;
                                   Group =:= http_protobuf ->
    application:stop(opentelemetry),
    application:stop(opentelemetry_exporter),
    ok;
end_per_group(_, _) ->
    application:stop(opentelemetry),
    application:unload(opentelemetry_exporter),
    ok.

verify_export(Config) ->
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=my-test-service,service.version=98da75ea6d38724743bf42b45565049238d86b3f,schema_url=https://opentelemetry.io/schemas/1.8.0"),
    Protocol = ?config(protocol, Config),
    Compression = ?config(compression, Config),

    Port = case Protocol of
               grpc ->
                   4317;
               http_protobuf ->
                   4318
           end,

    {ok, State} = otel_exporter_metrics_otlp:init(#{protocol => Protocol,
                                                    compression => Compression,
                                                    endpoints => [{http, "localhost", Port, []}]}),

    %% Tempoararily adding this because without this, we would face
    %% {error, no_endpoints} when attempt to export when we have more
    %% than 1 gprc test case.
    timer:sleep(800),

    Exemplars = [#exemplar{filtered_attributes=#{<<"a">> => <<"b">>},
                           time=opentelemetry:timestamp(),
                           value=3,
                           span_id=otel_id_generator:generate_span_id(),
                           trace_id=otel_id_generator:generate_trace_id()}],
    Metrics = [#metric{scope=#instrumentation_scope{name = <<"scope-1">>,
                                                    version = <<"version-1">>,
                                                    schema_url = <<"https://example.com/schemas/1.8.0">>},
                       name = <<"sum name">>,
                       description = <<"some sum description">>,
                       unit = kb,
                       data = #sum{aggregation_temporality = temporality_cumulative,
                                   is_monotonic=true,
                                   datapoints=[#datapoint{
                                                  attributes=otel_attributes:new(#{<<"key-1">> => <<"value-1">>},
                                                                                 128, 128),
                                                  start_time=opentelemetry:timestamp(),
                                                  time=opentelemetry:timestamp(),
                                                  value=5,
                                                  exemplars=Exemplars,
                                                  flags=0
                                                 },
                                               #datapoint{
                                                  attributes=otel_attributes:new(#{<<"key-2">> => <<"value-2">>},
                                                                                 128, 128),
                                                  start_time=opentelemetry:timestamp(),
                                                  time=opentelemetry:timestamp(),
                                                  value=8,
                                                  exemplars=[],
                                                  flags=0
                                                 }]}},
               #metric{scope=#instrumentation_scope{name = <<"scope-1">>,
                                                    version = <<"version-1">>,
                                                    schema_url = <<"https://example.com/schemas/1.8.0">>},
                       name = <<"gauge name">>,
                       description = <<"some gauge description">>,
                       unit = kb,
                       data = #gauge{datapoints=[#datapoint{
                                                    attributes=otel_attributes:new(#{<<"key-1">> => <<"value-1">>},
                                                                                   128, 128),
                                                    start_time=opentelemetry:timestamp(),
                                                    time=opentelemetry:timestamp(),
                                                    value=8,
                                                    exemplars=[],
                                                    flags=0
                                                   },
                                                 #datapoint{
                                                    attributes=otel_attributes:new(#{<<"key-2">> => <<"value-2">>},
                                                                                   128, 128),
                                                    start_time=opentelemetry:timestamp(),
                                                    time=opentelemetry:timestamp(),
                                                    value=9,
                                                    exemplars=[],
                                                    flags=0
                                                   }]}},
               #metric{scope=#instrumentation_scope{name = <<"scope-1">>,
                                                    version = <<"version-1">>,
                                                    schema_url = <<"https://example.com/schemas/1.8.0">>},
                       name = <<"histogram name">>,
                       description = <<"some histogram description">>,
                       unit = kb,
                       data = #histogram{aggregation_temporality = temporality_cumulative,
                                         datapoints=[#histogram_datapoint{
                                                        attributes=otel_attributes:new(#{<<"key-1">> => <<"value-1">>},
                                                                                       128, 128),
                                                        start_time=opentelemetry:timestamp(),
                                                        time=opentelemetry:timestamp(),
                                                        count = 3,
                                                        sum = 9,
                                                        bucket_counts = [1,1,1],
                                                        explicit_bounds = [0,3,5],
                                                        exemplars = [],
                                                        flags = 0,
                                                        min = 1,
                                                        max = 5
                                                       },
                                                     #histogram_datapoint{
                                                        attributes=otel_attributes:new(#{<<"key-2">> => <<"value-2">>},
                                                                                       128, 128),
                                                        start_time=opentelemetry:timestamp(),
                                                        time=opentelemetry:timestamp(),
                                                        count = 3,
                                                        sum = 9,
                                                        bucket_counts = [1,1,1],
                                                        explicit_bounds = [0,3,5],
                                                        exemplars = [],
                                                        flags = 0,
                                                        min = 1,
                                                        max = 5
                                                       }]}}
              ],
    Resource = otel_resource_env_var:get_resource([]),
    ?assertMatch(ok, otel_exporter_metrics_otlp:export(Metrics, Resource, State)),
    ok.

configuration(_Config) ->
    try
        ?assertMatch(#{endpoints := [#{host := "localhost", path := "/v1/metrics", port := 4318,
                                       scheme := "http"}]},
                     otel_exporter_metrics_otlp:merge_with_environment(#{}))
    after
        os:unsetenv("OTEL_EXPORTER_OTLP_ENDPOINT"),
        os:unsetenv("OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"),
        os:unsetenv("OTEL_EXPORTER_OTLP_HEADERS"),
        os:unsetenv("OTEL_EXPORTER_OTLP_METRICS_HEADERS"),
        os:unsetenv("OTEL_EXPORTER_OTLP_PROTOCOL")
    end.
