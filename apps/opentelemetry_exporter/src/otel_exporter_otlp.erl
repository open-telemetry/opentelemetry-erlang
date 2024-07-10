%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
%% @doc A module of functionality shared between OTLP exporters for
%% the various signals.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_exporter_otlp).

-export([init/1,
         export_http/6,
         export_grpc/5,
         endpoints/2,
         merge_with_environment/8]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_HTTP_PORT, 4318).
-define(DEFAULT_HTTP_ENDPOINTS, [#{host => "localhost",
                                   path => [],
                                   port => ?DEFAULT_HTTP_PORT,
                                   scheme => "http"}]).

-define(DEFAULT_GRPC_PORT, 4317).
-define(DEFAULT_GRPC_ENDPOINTS, [#{host => "localhost",
                                   path => [],
                                   port => ?DEFAULT_GRPC_PORT,
                                   scheme => "http"}]).

-type headers() :: [{unicode:chardata(), unicode:chardata()}].
-type endpoint() :: uri_string:uri_string() | uri_string:uri_map().
-type endpoint_map() :: #{scheme := unicode:chardata(),
                          host := unicode:chardata(),
                          path => unicode:chardata(),
                          port => integer(),
                          ssl_options => []}.

-type protocol() :: grpc | http_protobuf | http_json.
-type compression() :: gzip.

-type opts() :: #{endpoints => [endpoint()],
                  headers => headers(),
                  protocol => protocol(),
                  ssl_options => list()}.

-export_type([opts/0,
              headers/0,
              compression/0,
              endpoint_map/0,
              endpoint/0,
              protocol/0]).

-type state() :: #{channel := term() | undefined,
                   httpc_profile := atom() | undefined,
                   protocol := protocol(),
                   channel_pid := pid() | undefined,
                   headers := headers(),
                   compression := compression() | undefined,
                   grpc_metadata := map() | undefined,
                   endpoints := [endpoint_map()]}.

-include_lib("opentelemetry_api/include/gradualizer.hrl").

%% @doc Initialize the exporter based on the provided configuration.
-spec init(opts()) -> {ok, state()}.
init(Opts) ->
    State = #{channel => undefined,
              httpc_profile => undefined,
              protocol => http_protobuf,
              channel_pid => undefined,
              headers => [],
              compression => undefined,
              grpc_metadata => undefined,
              endpoints => []},

    SSLOptions = maps:get(ssl_options, Opts, undefined),
    Headers = headers(maps:get(headers, Opts, [])),
    Compression = maps:get(compression, Opts, undefined),
    case maps:get(protocol, Opts, http_protobuf) of
        grpc ->
            Endpoints = endpoints(maps:get(endpoints, Opts), SSLOptions),
            ChannelOpts = maps:get(channel_opts, Opts, #{}),
            UpdatedChannelOpts = case Compression of
                                   undefined -> ChannelOpts;
                                   Encoding -> maps:put(encoding, Encoding, ChannelOpts)
                                 end,

            %% Channel name can be any term. To separate Channels per
            %% the process calling the exporter  use the current pid
            Channel = self(),
            case grpcbox_channel:start_link(Channel,
                                            grpcbox_endpoints(Endpoints),
                                            UpdatedChannelOpts) of
                {ok, ChannelPid} ->
                    {ok, State#{channel => Channel,
                                channel_pid => ChannelPid,
                                endpoints => Endpoints,
                                headers => Headers,
                                compression => Compression,
                                grpc_metadata => headers_to_grpc_metadata(Headers),
                                protocol => grpc}};
                ErrorOrIgnore ->
                    %% TODO: do something different for `already_started' error?

                    %% even if it is `ignore' we should just use `http_protobuf' because
                    %% `ignore' should never happen and means something is wrong
                    ?LOG_WARNING("unable to start grpc channel for exporting and falling back "
                                 "to http_protobuf protocol. reason=~p", [ErrorOrIgnore]),
                    {ok, State#{endpoints => Endpoints,
                                headers => Headers,
                                compression => Compression,
                                protocol => http_protobuf}}
            end;
        http_protobuf ->
            HttpcProfile = start_httpc(Opts),
            Endpoints = endpoints(maps:get(endpoints, Opts), SSLOptions),
            {ok, State#{httpc_profile => HttpcProfile,
                        endpoints => Endpoints,
                        headers => Headers,
                        compression => Compression,
                        protocol => http_protobuf}};
        http_json ->
            HttpcProfile = start_httpc(Opts),
            Endpoints = endpoints(maps:get(endpoints, Opts), SSLOptions),
            {ok, State#{httpc_profile => HttpcProfile,
                        endpoints => Endpoints,
                        headers => Headers,
                        compression => Compression,
                        protocol => http_json}}
    end.

%% use a unique httpc profile per exporter
start_httpc(Opts) ->
    HttpcProfile = list_to_atom(lists:concat([?MODULE, "_", erlang:pid_to_list(self())])),

    case httpc:info(HttpcProfile) of
        {error, {not_started, _}} ->
            %% by default use inet6fb4 which will try ipv6 and then fallback to ipv4 if it fails
            HttpcOptions = lists:ukeymerge(1,
                                           lists:usort(maps:get(httpc_options, Opts, [])),
                                           [{ipfamily, inet6fb4}]
                                          ),
            %% can't use `stand_alone' because then `httpc:info(Profile)' would fail
            {ok, Pid} = inets:start(httpc, [{profile, HttpcProfile}]),
            ok = httpc:set_options(HttpcOptions, Pid);
        _ ->
            %% profile already started
            ok
    end,
    HttpcProfile.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export_http(Address, Headers, Body, Compression, SSLOptions, HttpcProfile) ->
    {NewHeaders, NewBody} =
        case Compression of
            gzip -> {[{"content-encoding", "gzip"} | Headers], zlib:gzip(Body)};
            _ -> {Headers, Body}
        end,

    case httpc:request(post, {Address, NewHeaders, "application/x-protobuf", NewBody},
                       [{ssl, SSLOptions}], [], HttpcProfile) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_INFO("error response from service exported to status=~p ~s",
                      [Code, Message]),
            error;
        {error, Reason} ->
            ?LOG_INFO("client error exporting ~p", [Reason]),
            error
    end.

export_grpc(GrpcCtx, GrpcServiceModule, Metadata, Request, Channel) ->
    GrpcCtx1 = grpcbox_metadata:append_to_outgoing_ctx(GrpcCtx, Metadata),
    case GrpcServiceModule:export(GrpcCtx1, Request, #{channel => Channel}) of
        {ok, _Response, _ResponseMetadata} ->
            ok;
        {error, {Status, Message}, _} ->
            ?LOG_INFO("OTLP grpc export failed with GRPC status ~s : ~s", [Status, Message]),
            error;
        {http_error, {Status, _}, _} ->
            ?LOG_INFO("OTLP grpc export failed with HTTP status code ~s", [Status]),
            error;
        {error, Reason} ->
            ?LOG_INFO("OTLP grpc export failed with error: ~p", [Reason]),
            error
    end.

grpcbox_endpoints(Endpoints) ->
    [{scheme(Scheme), Host, Port, maps:get(ssl_options, Endpoint, [])} ||
        #{scheme := Scheme, host := Host, port := Port} = Endpoint <- Endpoints].

headers_to_grpc_metadata(Headers) ->
    lists:foldl(fun({X, Y}, Acc) ->
                        maps:put(unicode:characters_to_binary(X), unicode:characters_to_binary(Y), Acc)
                end, #{}, Headers).

%% make all headers into list strings
headers(List) when is_list(List) ->
    Headers =[{unicode:characters_to_list(X), unicode:characters_to_list(Y)} || {X, Y} <- List],
    add_user_agent(Headers);
headers(_) ->
    add_user_agent([]).

add_user_agent(Headers) ->
    case lists:search(fun({Header, _}) -> string:to_lower(Header) == "user-agent" end, Headers) of
        {value, _} -> Headers;
        false -> [{"User-Agent", user_agent()} | Headers]
    end.

user_agent() ->
    {ok, ExporterVsn} = application:get_key(opentelemetry_exporter, vsn),
    lists:flatten(io_lib:format("OTel-OTLP-Exporter-erlang/~s", [ExporterVsn])).

-spec endpoints([endpoint()], list() | undefined) -> [endpoint_map()].
endpoints(List, DefaultSSLOpts) when is_list(List) ->
    Endpoints = case io_lib:printable_list(List) of
                    true ->
                        [List];
                    false ->
                        List
                end,

    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, Endpoints);
endpoints(Endpoint, DefaultSSLOpts) ->
    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, [Endpoint]).

endpoint(Endpoint, DefaultSSLOpts) ->
    case parse_endpoint(Endpoint, DefaultSSLOpts) of
        false ->
            ?LOG_WARNING("Failed to parse and ignoring exporter endpoint ~p", [Endpoint]),
            false;
        Parsed ->
            Parsed
    end.

parse_endpoint({Scheme, Host, Port, SSLOptions}, _DefaultSSLOpts) when is_list(SSLOptions) ->
    {true, #{scheme => atom_to_list(Scheme),
             host => unicode:characters_to_list(Host),
             port => Port,
             path => [],
             ssl_options => SSLOptions}};
parse_endpoint({Scheme, Host, Port, _}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    {true, #{scheme => atom_to_list(Scheme),
             host => HostString,
             port => Port,
             path => [],
             ssl_options => update_ssl_opts(HostString, DefaultSSLOpts)}};
parse_endpoint(Endpoint=#{host := Host,
                          scheme := Scheme,
                          path := Path}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    %% `merge' keeps the value in the second argument if the key is in both
    %% so to always update the scheme/host/port to charlists we set those
    %% separate from port/ssl_options which should only be added if not already
    %% found in `Endpoint'
    {true, maps:merge(#{port => scheme_port(Scheme),
                        %% we only want to run `tls_certificate_check' if absolutely
                        %% necessary, so wrapping the setup of ssl options here
                        %% in a check that it won't just be overwritten by the value
                        %% from the Endpoint anyway
                        ssl_options => case maps:is_key(ssl_options, Endpoint) of
                                           true ->
                                               [];
                                           false ->
                                               update_ssl_opts(HostString, DefaultSSLOpts)
                                       end},
                      Endpoint#{scheme => to_charlist(Scheme),
                                host => HostString,
                                path => unicode:characters_to_list(Path)})};
parse_endpoint(String, DefaultSSLOpts) when is_list(String) orelse is_binary(String) ->
    case unicode:characters_to_list(String) of
        {_, _, _} ->
            ?LOG_WARNING("error converting endpoint URI ~s to utf8", [String]),
            false;
        UnicodeList ->
            case uri_string:parse(UnicodeList) of
                {error, Reason, Message} ->
                    ?LOG_WARNING("error parsing endpoint URI: ~s : ~p", [Reason, Message]),
                    false;
                ParsedUri ->
                    ParsedUri1 = maybe_add_scheme_port(ParsedUri),
                    parse_endpoint(ParsedUri1, DefaultSSLOpts)
            end
    end;
parse_endpoint(_, _) ->
    false.

to_charlist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_charlist(Other) ->
    unicode:characters_to_list(Other).

scheme_port(Scheme) when not is_atom(Scheme) ->
    scheme_port(scheme(Scheme));
scheme_port(http) ->
    80;
scheme_port(https) ->
    443;
scheme_port(_) ->
    %% unknown scheme
    80.

maybe_add_scheme_port(Uri=#{port := _Port}) ->
    Uri;
maybe_add_scheme_port(Uri=#{scheme := "http"}) ->
    Uri#{port => 80};
maybe_add_scheme_port(Uri=#{scheme := "https"}) ->
    Uri#{port => 443};
%% an unknown scheme
maybe_add_scheme_port(Uri) ->
    Uri.


%% if no ssl opts are defined by the user then use defaults from `tls_certificate_check'
update_ssl_opts(Host, undefined) ->
    tls_certificate_check:options(Host);
update_ssl_opts(_, SSLOptions) ->
    SSLOptions.


scheme(Scheme) when Scheme =:= "https" orelse Scheme =:= <<"https">> ->
    https;
scheme(Scheme) when Scheme =:= "http" orelse Scheme =:= <<"http">> ->
    http;
scheme(Scheme) ->
    ?LOG_WARNING("unknown scheme ~p, converting to existing atom, if possible, and using as is", [Scheme]),
    to_existing_atom(Scheme).

to_existing_atom(Term) when is_atom(Term) ->
    Term;
to_existing_atom(Scheme) when is_list(Scheme) ->
    list_to_existing_atom(Scheme);
to_existing_atom(Scheme) when is_binary(Scheme) ->
    %% TODO: switch to binary_to_existing_atom once we drop OTP-22 support
    list_to_existing_atom(binary_to_list(Scheme));
to_existing_atom(_) ->
    erlang:error(bad_exporter_scheme).

merge_with_environment(ConfigMapping, AppEnv, Opts, SignalEndpointConfigKey, SignalHeadersConfigKey, SignalProtocolConfigKey, SignalCompressionConfigKey, DefaultPath) ->
    Config = #{otlp_endpoint => undefined,
               SignalEndpointConfigKey => undefined,
               otlp_headers => undefined,
               SignalHeadersConfigKey => undefined,
               otlp_protocol => undefined,
               SignalProtocolConfigKey => undefined,
               otlp_compression => undefined,
               SignalCompressionConfigKey => undefined,
               ssl_options => undefined},

    AppOpts = otel_configuration:merge_list_with_environment(ConfigMapping, AppEnv, Config),

    %% check for error in app env value parsing
    case maps:get(otlp_endpoint, AppOpts) of
        {error, Reason, Message} ->
            ?LOG_WARNING("error parsing endpoint URI: ~s : ~p", [Reason, Message]),
            maps:put(endpoints, [],
                     maps:put(endpoints, [], Opts));

        _ ->
            Opts1 = update_opts(otlp_protocol, protocol, http_protobuf, AppOpts, Opts),
            Opts2 = update_opts(SignalProtocolConfigKey, protocol, http_protobuf, AppOpts, Opts1),

            %% append the default path `/v1/<signal>` only to the path of otlp_endpoint
            Opts3 = update_opts(otlp_endpoint, endpoints, default_endpoints_for_protocol(Opts2), AppOpts, Opts2, fun maybe_to_list/1),
            DefaultSignalEndpoints = endpoints_append_path(maps:get(endpoints, Opts3), maps:get(protocol, Opts3), DefaultPath),

            %% now set `endpoints' to either the default signal value or the user configured value
            Opts4 = update_opts(SignalEndpointConfigKey,
                                endpoints,
                                DefaultSignalEndpoints,
                                AppOpts,
                                Opts3#{endpoints => DefaultSignalEndpoints},
                                fun maybe_to_list/1),

            Opts5 = update_opts(otlp_headers, headers, [], AppOpts, Opts4),
            Opts6 = update_opts(SignalHeadersConfigKey, headers, [], AppOpts, Opts5),

            Opts7 = update_opts(otlp_compression, compression, undefined, AppOpts, Opts6),
            Opts8 = update_opts(SignalCompressionConfigKey, compression, undefined, AppOpts, Opts7),

            update_opts(ssl_options, ssl_options, undefined, AppOpts, Opts8)
    end.

default_endpoints_for_protocol(Opts) ->
    case maps:get(protocol, Opts) of
        http_protobuf ->
            ?DEFAULT_HTTP_ENDPOINTS;
        grpc ->
            ?DEFAULT_GRPC_ENDPOINTS
    end.

maybe_to_list(E) when is_list(E) ->
    case io_lib:printable_list(E) of
        true ->
            [E];
        false ->
            E
    end;
maybe_to_list(E) ->
    [E].

endpoints_append_path(Endpoints, http_protobuf, DefaultPath) ->
    endpoints_append_path_(Endpoints, DefaultPath);
endpoints_append_path(Endpoints, _, _) ->
    endpoints_append_path_(Endpoints, "").

endpoints_append_path_(E, DefaultPath) when is_list(E) ->
    case io_lib:printable_list(E) of
        true ->
            [append_path(E, DefaultPath)];
        false ->
            [append_path(Endpoint, DefaultPath) || Endpoint <- E]
    end;
endpoints_append_path_(E, DefaultPath) ->
    [append_path(E, DefaultPath)].

append_path({Scheme, Host, Port, SSLOptions}, DefaultPath) ->
    #{scheme => atom_to_list(Scheme),
      host => Host,
      port => Port,
      path => filename:join([], DefaultPath),
      ssl_options => SSLOptions};
append_path(Endpoint=#{path := Path}, DefaultPath) ->
    Endpoint#{path => filename:join(Path, DefaultPath)};
append_path(Endpoint=#{}, DefaultPath) ->
    Endpoint#{path => filename:join([], DefaultPath)};
append_path(EndpointString, DefaultPath) when is_list(EndpointString) orelse is_binary(EndpointString) ->
    Endpoint=#{path := Path} = uri_string:parse(EndpointString),
    Endpoint#{path => filename:join(?assert_type(Path, string() | binary()), DefaultPath)}.

%% use the value from the environment if it exists, otherwise use the value
%% passed in Opts or the default
update_opts(AppKey, OptKey, Default, AppOpts, Opts) ->
    update_opts(AppKey, OptKey, Default, AppOpts, Opts, fun id/1).

update_opts(AppKey, OptKey, Default, AppOpts, Opts, Transform) ->
    case maps:get(AppKey, AppOpts) of
        undefined ->
            %% use default unless already set, in which case just transform
            maps:update_with(OptKey, Transform, Default, Opts);
        EnvValue ->
            maps:put(OptKey, Transform(EnvValue), Opts)
    end.

id(X) ->
    X.

