%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%% @doc Resource detectors are responsible for reading in attributes about
%% the runtime environment of a node (such as an environment variable or
%% some metadata endpoint provided by a cloud host) and returning a
%% {@link otel_resource:t()} made from those attributes.
%%
%% This module is meant for users who intend to write their own resource
%% detectors.
%%
%% This behaviour is a state machine (started by the `opentelemetry' application)
%%  which spawns a process for each detector, collects the
%% results of running each, and merges them in the order they are defined. Once the
%% state machine process is ready, it will reply to {@link get_resource/1}
%% calls with the final {@link otel_resource:t()}.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_resource_detector).

-behaviour(gen_statem).

-export([start_link/1,
         get_resource/0,
         get_resource/1]).

-export([init/1,
         callback_mode/0,
         handle_event/4]).

-callback get_resource(term()) -> otel_resource:t().
%% Function that takes the configuration of a resource detector 
%% and returns a resource.

-type detector() :: module() | {module(), term()}.

-include_lib("kernel/include/logger.hrl").

-record(data, {resource            :: otel_resource:t(),
               configured_resource :: otel_resource:t(),
               detectors           :: [detector()],
               detector_timeout    :: integer()}).

%% @private
-spec start_link(otel_configuration_sdk:configuration()) ->
          {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @equiv get_resource(6000)
-spec get_resource() -> otel_resource:t().
get_resource() ->
    get_resource(6000).

%% @doc Gets the resource formed by detecting attributes through resource
%% detectors.
%%
%% If the call doesn't complete within the given `Timeout' then an empty
%% resource is returned.
-spec get_resource(timeout()) -> otel_resource:t().
get_resource(Timeout) ->
    try gen_statem:call(?MODULE, get_resource, Timeout)
    catch
        _:_ ->
            %% TODO: should we return an error instead?
            %% returning an empty resource ensures we continue on and
            %% don't crash anything depending on the returned resource
            %% but could mean we have an empty resource while the
            %% gen_server later has a full resourced
            otel_resource:create([])
    end.

%% @private
-spec init(otel_configuration_sdk:configuration()) -> gen_statem:init_result(collecting).
init(Config) ->
    process_flag(trap_exit, true),

    Erlang = otel_configuration_sdk:erlang_distribution(Config),
    Detectors = otel_configuration_sdk:value(resource_detectors, Erlang, []),
    DetectorTimeout = otel_configuration_sdk:value(resource_detector_timeout,
                                                    Erlang,
                                                    5000),
    ConfiguredResource = case otel_configuration_sdk:resource(Config) of
                             undefined -> otel_resource:create([]);
                             Resource -> Resource
                         end,

    {ok, collecting, #data{resource=otel_resource:create([]),
                           configured_resource=ConfiguredResource,
                           detectors=Detectors,
                           detector_timeout=DetectorTimeout},
     [{next_event, internal, spawn_detectors}]}.

%% @private
callback_mode() ->
    [handle_event_function, state_enter].

%% @private
handle_event(enter, _, ready,
             Data=#data{resource=DetectedResource,
                        configured_resource=ConfiguredResource}) ->
    Defaults = default_resource_attributes(otel_resource:create([])),
    Resource = otel_resource:merge(DetectedResource, Defaults),
    {keep_state, Data#data{resource=otel_resource:merge(ConfiguredResource, Resource)}};
handle_event(enter, _, _, _) ->
    keep_state_and_data;
handle_event(internal, spawn_detectors, collecting, Data=#data{detectors=Detectors}) ->
    %% merging must be done in a specific order so Refs are kept in a list
    ToCollectRefs = spawn_detectors(Detectors),
    {next_state, next_state(ToCollectRefs), Data, [state_timeout(Data)]};
handle_event(info, {'EXIT', Pid, _}, {collecting, [{_, Pid, Detector} | Rest]}, Data) ->
    ?LOG_WARNING("detector ~p crashed while executing", [Detector]),
    {next_state, next_state(Rest), Data, [state_timeout(Data)]};

handle_event(info, {resource, Ref, Resource}, {collecting, [{Ref, _, _} | Rest]},
             Data=#data{resource=CurrentResource}) ->
    NewResource = otel_resource:merge(Resource, CurrentResource),
    {next_state, next_state(Rest), Data#data{resource=NewResource}, state_timeout(Data)};
handle_event(state_timeout, resource_detector_timeout, {collecting, [{_, Pid, Detector} | Rest]}, Data) ->
    ?LOG_WARNING("detector ~p timed out while executing", [Detector]),
    %% may still have an EXIT in the mailbox but with `unlink' we might not
    erlang:unlink(Pid),
    erlang:exit(Pid, kill),
    {next_state, next_state(Rest), Data, state_timeout(Data)};
handle_event(info, _, _, _Data) ->
    %% merging resources must be done in order, so postpone the message
    %% if it isn't the head of the list
    {keep_state_and_data, [postpone]};

handle_event({call, From}, get_resource, ready, #data{resource=Resource}) ->
    {keep_state_and_data, [{reply, From, Resource}]};
handle_event({call, _From}, get_resource, _, _Data) ->
    %% can't get the resource until all detectors have completed
    %% at which point this statem will be in the `ready' state
    {keep_state_and_data, [postpone]};
handle_event(_, _, ready, _) ->
    %% if in `ready' state get rid of all the postpones messages in
    %% the mailbox that were postponed. Could be EXIT's or late resource
    %% messages
    keep_state_and_data.

%%

%% go to the `ready' state if the list of detectors to collect for is empty
next_state([]) ->
    ready;
next_state(List) ->
    {collecting, List}.

state_timeout(#data{detector_timeout=DetectorTimeout}) ->
    {state_timeout, DetectorTimeout, resource_detector_timeout}.

spawn_detectors(Detectors) ->
    lists:map(fun(Detector) ->
                      Ref = erlang:make_ref(),
                      Pid = spawn_detector(Detector, Ref),
                      {Ref, Pid, Detector}
              end, Detectors).

spawn_detector(Detector={Module, Config}, Ref) ->
    Self = self(),
    erlang:spawn_link(fun() ->
                              try Module:get_resource(Config) of
                                  Resource ->
                                      Self ! {resource, Ref, Resource}
                              catch
                                  C:T:S ->
                                      ?LOG_WARNING("caught exception while detector ~p was "
                                                   "executing: class=~p exception=~p stacktrace=~p",
                                                   [Detector, C, T, S]),
                                      %% TODO: log about detector's exception
                                      Self ! {resource, Ref, otel_resource:create([])}
                              end
                      end);
spawn_detector(Module, Ref) ->
    spawn_detector({Module, []}, Ref).

default_resource_attributes(Resource) ->
    ProgName = prog_name(),
    ProcessResource = otel_resource:create([{'process.executable.name', ProgName} | process_attributes()]),
    Resource1 = otel_resource:merge(ProcessResource, Resource),
    Resource2 = add_service_name(Resource1, ProgName),
    Resource3 = add_service_instance(Resource2),
    add_telemetry_info(Resource3).

process_attributes() ->
    OtpVsn = otp_vsn(),
    ErtsVsn = erts_vsn(),
    [{'process.runtime.name', unicode:characters_to_binary(emulator())},
     {'process.runtime.version', unicode:characters_to_binary(ErtsVsn)},
     {'process.runtime.description', unicode:characters_to_binary(runtime_description(OtpVsn, ErtsVsn))}].

runtime_description(OtpVsn, ErtsVsn) ->
    io_lib:format("Erlang/OTP ~s erts-~s", [OtpVsn, ErtsVsn]).

erts_vsn() ->
    erlang:system_info(version).

otp_vsn() ->
    erlang:system_info(otp_release).

emulator() ->
    erlang:system_info(machine).

prog_name() ->
    %% RELEASE_PROG is set by mix and rebar3 release scripts
    %% PROGNAME is an OS variable set by `erl' and rebar3 release scripts
    unicode:characters_to_binary(os_or_default("RELEASE_PROG", os_or_default("PROGNAME", <<"erl">>))).

os_or_default(EnvVar, Default) ->
    case os:getenv(EnvVar) of
        false ->
            Default;
        Value ->
            Value
    end.

-dialyzer({nowarn_function, find_release/0}).
find_release() ->
    try release_handler:which_releases(permanent) of
        [{RelName, RelVsn, _Apps, permanent} | _] ->
            {RelName, RelVsn}
    catch
        %% can happen if `release_handler' isn't available
        %% or its process isn't started
        _:_ ->
            {release_name(), os:getenv("RELEASE_VSN")}
    end.

release_name() ->
    case os:getenv("RELEASE_NAME") of
        false ->
            %% older relx generated releases only set and export this variable
            os:getenv("REL_NAME");
        RelName ->
            RelName
    end.

%% Check for service.name in attributes, then use the release name or the SDK
%% default. Environment substitution belongs to preprocessing of declarative
%% configuration and is deliberately not performed here.
add_service_name(Resource, ProgName) ->
    case otel_resource:is_key('service.name', Resource) of
        false ->
            ServiceResource = service_release_name(ProgName),
            otel_resource:merge(ServiceResource, Resource);
        true ->
            Resource
    end.

%% If service.instance.id is absent, derive it from the node name or generate it.
%% if node is nonode@nohost set a random instance id
add_service_instance(Resource) ->
    case otel_resource:is_key('service.instance.id', Resource) of
        false ->
            ServiceInstanceId = case erlang:node() of
                                    nonode@nohost ->
                                        otel_id_generator:generate_trace_id();
                                    ServiceInstance ->
                                        ServiceInstance1 = erlang:atom_to_binary(
                                                             ServiceInstance, utf8),
                                        case binary:match(ServiceInstance1,
                                                          <<"@localhost">>) of
                                            nomatch -> ServiceInstance1;
                                            _ -> otel_id_generator:generate_trace_id()
                                        end
                                end,
            ServiceInstanceResource = otel_resource:create(
                                        [{'service.instance.id', ServiceInstanceId}]),
            otel_resource:merge(ServiceInstanceResource, Resource);
        true ->
            Resource
    end.

service_release_name(ProgName) ->
    case find_release() of
        {RelName, RelVsn} when RelName =/= false ->
            otel_resource:create([{'service.name', RelName} |
                                  case RelVsn of
                                      false -> [];
                                      _ -> [{'service.version', RelVsn}]
                                  end]);
        _ ->
            otel_resource:create([{'service.name', <<"unknown_service:", ProgName/binary>>}])
    end.

add_telemetry_info(Resource) ->
    {ok, LibraryVsn} = application:get_key(opentelemetry, vsn),
    LibraryName = <<"opentelemetry">>,
    LibraryLanguage = <<"erlang">>,
    ResourceAttributes = [{'telemetry.sdk.name', LibraryName},
                          {'telemetry.sdk.language', LibraryLanguage},
                          {'telemetry.sdk.version', LibraryVsn}],
    TelemetryInfoResource = otel_resource:create(ResourceAttributes),
    otel_resource:merge(TelemetryInfoResource, Resource).
