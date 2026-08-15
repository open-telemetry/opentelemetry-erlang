%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_otlp_logs).

-export([to_proto/3]).

%% for testing
-ifdef(TEST).
-export([to_proto_by_instrumentation_scope/2]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(IS_STRING(String),
        (is_list(String) orelse is_binary(String))).

to_proto([], _Resource, _Config) ->
    empty;
to_proto(Logs, Resource, Config) ->
    InstrumentationScopeLogs = to_proto_by_instrumentation_scope(Logs, Config),
    Attributes = otel_resource:attributes(Resource),
    ResourceLogs = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                   dropped_attributes_count => otel_attributes:dropped(Attributes)},
                     scope_logs => InstrumentationScopeLogs},
    case otel_resource:schema_url(Resource) of
        undefined ->
            #{resource_logs => [ResourceLogs]};
        SchemaUrl ->
            #{resource_logs => [ResourceLogs#{schema_url => SchemaUrl}]}
    end.


to_proto_by_instrumentation_scope(Logs, Config) ->
    ScopeLogs = logs_by_scope(Logs, Config),
    maps:fold(fun(Scope, LogRecords, Acc) ->
                      [#{scope => otel_otlp_common:to_instrumentation_scope_proto(Scope),
                         log_records => LogRecords
                         %% schema_url              => unicode:chardata() % = 3, optional
                        } | Acc]
              end, [], ScopeLogs).


logs_by_scope(ScopeLogs, Config) ->
    maps:fold(fun(InstrumentationScope, Logs, Acc) ->
                      LogRecords = [log_record(Log, Config) || Log <- Logs],
                      Acc#{InstrumentationScope => LogRecords}
              end, #{}, ScopeLogs).


log_record(#{level := Level,
             msg := Body,
             meta := Metadata=#{time := ObservedTime}}, Config) ->
    Time = opentelemetry:timestamp(),
    {SeverityNumber, SeverityText} = level_to_severity(Level),
    Body1 = case format_msg(Body, Metadata, Config) of
                S when ?IS_STRING(S) ->
                    %% if body is a string, make it a single line
                    T = lists:reverse(
                          trim(
                            lists:reverse(
                              trim(S, false)), true)),
                    re:replace(T,",?\r?\n\s*",", ",
                               [{return,list}, global, unicode]);
                M ->
                    M
            end,
    Attributes = maps:without([gl, time, report_cb], Metadata),
    Attributes1 = maps:fold(fun(K, V, Acc) ->
                                    [#{key => otel_otlp_common:to_binary(K),
                                       value => otel_otlp_common:to_any_value(V)} | Acc]
                            end, [], Attributes),
    DroppedAttributesCount = maps:size(Attributes) - length(Attributes1),
    Flags = 0,

    %% Note: otel_trace_id and otel_span_id from hex_span_ctx are now binaries, not charlists.
    %% They are hex encoded, but the OTLP trace_id and span_id fields are the
    %% raw 16/8 bytes, so they must be decoded before being put in the
    %% LogRecord. If either id can't be decoded then the correlation fields
    %% are omitted entirely rather than exporting garbage ids.
    LogRecord = case Metadata of
        #{otel_trace_id := MetaTraceId,
          otel_span_id := MetaSpanId} ->
            correlation_fields(MetaTraceId, MetaSpanId,
                               maps:get(otel_trace_flags, Metadata, undefined));
        _ ->
            #{}
    end,



    LogRecord#{time_unix_nano          => opentelemetry:timestamp_to_nano(Time),
               observed_time_unix_nano => erlang:convert_time_unit(ObservedTime, microsecond, nanosecond),
               severity_number         => SeverityNumber,
               severity_text           => SeverityText,
               body                    => otel_otlp_common:to_any_value(Body1),
               attributes              => Attributes1,
               dropped_attributes_count => DroppedAttributesCount,
               flags                   => Flags
              }.

%% Decode a trace or span id from logger metadata into the raw bytes the
%% OTLP LogRecord expects. The metadata value is a lowercase hex encoded
%% binary when set by `otel_span:hex_span_ctx/1' (a hex encoded charlist
%% in older versions of the API) but raw ids are also accepted in case
%% the user set the metadata themselves.
%% Decode both correlation ids together: either both are valid and the
%% correlation fields are returned (trace_flags added only when the
%% metadata carried it), or all correlation fields are omitted.
correlation_fields(MetaTraceId, MetaSpanId, TraceFlagsHex) ->
    case {decode_id(MetaTraceId, 16), decode_id(MetaSpanId, 8)} of
        {{ok, TraceId}, {ok, SpanId}} ->
            Base = #{trace_id => TraceId,
                     span_id => SpanId},
            case decode_trace_flags(TraceFlagsHex) of
                undefined ->
                    Base;
                TraceFlags ->
                    Base#{trace_flags => TraceFlags}
            end;
        _ ->
            #{}
    end.

%% Absent flags metadata omits the field entirely; present but
%% malformed flags (wrong size, non-binary, non-hex digits) fall back
%% to 0 rather than raising from binary_to_integer/2.
decode_trace_flags(undefined) ->
    undefined;
decode_trace_flags(TraceFlagsHex) when is_binary(TraceFlagsHex),
                                       byte_size(TraceFlagsHex) =:= 2 ->
    try
        erlang:binary_to_integer(TraceFlagsHex, 16)
    catch
        _:_ ->
            0
    end;
decode_trace_flags(_) ->
    0.

-spec decode_id(term(), pos_integer()) -> {ok, binary()} | error.
decode_id(Id, NumBytes) when is_list(Id) ->
    %% logger metadata is arbitrary user data: characters_to_binary/1
    %% raises badarg for non-chardata lists (improper lists, atoms in
    %% lists), which must omit the ids rather than crash the export.
    try unicode:characters_to_binary(Id) of
        Bin when is_binary(Bin) ->
            decode_id(Bin, NumBytes);
        _ ->
            error
    catch
        _:_ ->
            error
    end;
decode_id(Id, NumBytes) when is_binary(Id), byte_size(Id) =:= NumBytes ->
    %% already the raw bytes
    {ok, Id};
decode_id(Id, NumBytes) when is_binary(Id), byte_size(Id) =:= 2 * NumBytes ->
    try
        {ok, decode_hex(Id)}
    catch
        _:_ ->
            error
    end;
decode_id(_, _) ->
    error.

%% binary:decode_hex/1 requires OTP 24 and only accepts mixed case hex
%% digits starting from OTP 26.2, so decode by hand like
%% otel_utils:encode_hex/1 does for encoding on older OTP versions.
decode_hex(Hex) ->
    << <<(hex_nibble(Nibble)):4>> || <<Nibble>> <= Hex >>.

hex_nibble(N) when N >= $0, N =< $9 -> N - $0;
hex_nibble(N) when N >= $a, N =< $f -> N - $a + 10;
hex_nibble(N) when N >= $A, N =< $F -> N - $A + 10.

format_msg({string, Chardata}, Meta, Config) ->
    format_msg({"~ts", [Chardata]}, Meta, Config);
format_msg({report,_}=Msg, Meta, #{report_cb := Fun}=Config)
  when is_function(Fun,1); is_function(Fun,2) ->
    format_msg(Msg, Meta#{report_cb => Fun}, maps:remove(report_cb,Config));
format_msg({report, Report}, #{report_cb := Fun}=Meta, Config) when is_function(Fun, 1) ->
    try Fun(Report) of
        {Format, Args} when is_list(Format), is_list(Args) ->
            format_msg({Format,Args},maps:remove(report_cb,Meta),Config);
        Other ->
            format_msg({"REPORT_CB/1 ERROR: ~0tp; Returned: ~0tp",
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            format_msg({"REPORT_CB/1 CRASH: ~0tp; Reason: ~0tp",
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg({report, Report},#{report_cb := Fun}=Meta, Config) when is_function(Fun,2) ->
    try Fun(Report,maps:with([depth,chars_limit,single_line], Config)) of
        Chardata when ?IS_STRING(Chardata) ->
            try chardata_to_list(Chardata) % already size limited by report_cb
            catch _:_ ->
                    format_msg({"REPORT_CB/2 ERROR: ~0tp; Returned: ~0tp",
                                [Report,Chardata]},Meta,Config)
            end;
        Other ->
            format_msg({"REPORT_CB/2 ERROR: ~0tp; Returned: ~0tp",
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            format_msg({"REPORT_CB/2 CRASH: ~0tp; Reason: ~0tp",
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg({report, Report}, _Meta, _Config) ->
    %% report must be a map or key-value list so just return it as is
    %% if the user doesn't supply a `report_cb' function. this makes
    %% the LogRecord body also a list of key-value pairs
    Report;
format_msg(Msg, _Meta, #{depth := Depth,
                         chars_limit := CharsLimit,
                         single_line := Single}) ->
    Opts = chars_limit_to_opts(CharsLimit),
    format_msg(Msg, Depth, Opts, Single);
format_msg(Msg, _Meta, _) ->
    Opts = chars_limit_to_opts(unlimited),
    format_msg(Msg, unlimited, Opts, true).


format_msg({Format0,Args},Depth,Opts,Single) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format = reformat(Format1, Depth, Single),
        io_lib:build_text(Format,Opts)
    catch C:R:S ->
            FormatError = "FORMAT ERROR: ~0tp - ~0tp",
            case Format0 of
                FormatError ->
                    %% already been here - avoid failing cyclically
                    erlang:raise(C,R,S);
                _ ->
                    format_msg({FormatError,[Format0,Args]},Depth,Opts,Single)
            end
    end.

trim([H|T],Rev) when H==$\s; H==$\r; H==$\n ->
    trim(T,Rev);
trim([H|T],false) when is_list(H) ->
    case trim(H,false) of
        [] ->
            trim(T,false);
        TrimmedH ->
            [TrimmedH|T]
    end;
trim([H|T],true) when is_list(H) ->
    case trim(lists:reverse(H),true) of
        [] ->
            trim(T,true);
        TrimmedH ->
            [lists:reverse(TrimmedH)|T]
    end;
trim(String,_) ->
    String.

reformat(Format,unlimited,false) ->
    Format;
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $p ->
    [limit_depth(M#{width => 0}, Depth)|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $P ->
    [M#{width => 0}|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, Single) when C =:= $p; C =:= $w ->
    [limit_depth(M, Depth)|reformat(T, Depth, Single)];
reformat([H|T], Depth, Single) ->
    [H|reformat(T, Depth, Single)];
reformat([], _, _) ->
    [].

limit_depth(M0, unlimited) ->
    M0;
limit_depth(#{control_char:=C0, args:=Args}=M0, Depth) ->
    C = C0 - ($a - $A),				%To uppercase.
    M0#{control_char:=C,args:=Args++[Depth]}.

chars_limit_to_opts(unlimited) -> [];
chars_limit_to_opts(CharsLimit) -> [{chars_limit,CharsLimit}].

chardata_to_list(Chardata) ->
    case unicode:characters_to_list(Chardata,unicode) of
        List when is_list(List) ->
            List;
        Error ->
            throw(Error)
    end.

level_to_severity(emergency)->
    {'SEVERITY_NUMBER_FATAL', <<"SEVERITY_NUMBER_FATAL">>};
level_to_severity(alert)->
    {'SEVERITY_NUMBER_ERROR3', <<"SEVERITY_NUMBER_ERROR3">>};
level_to_severity(critical)->
    {'SEVERITY_NUMBER_ERROR2', <<"SEVERITY_NUMBER_ERROR2">>};
level_to_severity(error)->
    {'SEVERITY_NUMBER_ERROR', <<"SEVERITY_NUMBER_ERROR">>};
level_to_severity(warning)->
    {'SEVERITY_NUMBER_WARN', <<"SEVERITY_NUMBER_WARN">>};
level_to_severity(notice)->
    {'SEVERITY_NUMBER_INFO2', <<"SEVERITY_NUMBER_INFO2">>};
level_to_severity(info)->
    {'SEVERITY_NUMBER_INFO', <<"SEVERITY_NUMBER_INFO">>};
level_to_severity(debug)->
    {'SEVERITY_NUMBER_DEBUG', <<"SEVERITY_NUMBER_DEBUG">>}.
