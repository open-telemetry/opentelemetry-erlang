%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% Shared configuration test fixtures and assertions.
-module(otel_configuration_test_utils).

-export([resolve_declarative/1, configured_resource/1, resource_attributes/1,
         save_environment/0, restore_environment/1,
         add_warning_handler/2, log/2, warnings/0, foreach_case/2]).

-spec resolve_declarative(map()) ->
          {ok, otel_configuration_sdk:configuration()} |
          {error, otel_configuration_model:error_reason() | otel_configuration_sdk:error_reason()}.
resolve_declarative(Configuration) ->
    case otel_configuration_model:from_map(Configuration) of
        {ok, Model} -> otel_configuration_sdk:create(Model);
        {error, _}=Error -> Error
    end.

-spec configured_resource(otel_configuration_sdk:configuration()) -> otel_resource:t().
configured_resource(Configuration) ->
    case otel_configuration_sdk:resource(Configuration) of
        undefined -> error(missing_resource);
        Resource -> Resource
    end.

resource_attributes(undefined) -> error(missing_resource);
resource_attributes(Resource) ->
    case otel_resource:attributes(Resource) of
        undefined -> error(missing_resource);
        Attributes -> otel_attributes:map(Attributes)
    end.

%% Save and clear all OTEL variables, including ones not exercised by a suite.
%% Teardown also clears variables introduced during the test.
save_environment() ->
    Saved = otel_environment(),
    [os:unsetenv(Name) || {Name, _} <- Saved],
    Saved.

restore_environment(Saved) ->
    [os:unsetenv(Name) || {Name, _} <- otel_environment()],
    [os:putenv(Name, Value) || {Name, Value} <- Saved],
    ok.

otel_environment() ->
    [{Name, Value} || Entry <- os:getenv(), lists:prefix("OTEL_", Entry),
                      [Name, Value] <- [string:split(Entry, "=", leading)]].

add_warning_handler(Handler, MetadataKey) ->
    logger:add_handler(Handler, ?MODULE,
                       #{level => warning, config => #{pid => self(), key => MetadataKey}}).

%% Logger handlers run in the logging process. Only capture warnings originating
%% in this test process, so unrelated application warnings cannot race assertions.
log(#{meta := #{pid := Pid}=Meta}, #{config := #{pid := Pid, key := Key}}) ->
    case maps:find(Key, Meta) of
        {ok, Value} -> Pid ! {configuration_test_warning, Value};
        error -> ok
    end,
    ok;
log(_, _) -> ok.

warnings() ->
    receive
        {configuration_test_warning, Value} -> [Value | warnings()]
    after 0 -> []
    end.

%% Include the exact table row in a failure while retaining its original stack.
foreach_case(Check, Cases) ->
    lists:foreach(
      fun(Case) ->
              try Check(Case)
              catch
                  Class:Reason:Stack ->
                      erlang:raise(Class, {configuration_case_failed, Case, Reason}, Stack)
              end
      end, Cases).
