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
%% @doc
%% @end
%%%-----------------------------------------------------------------------
-module(otel_utils).

-export([format_exception/3,
         format_binary_string/2,
         format_binary_string/3,
         assert_to_binary/1]).

-include_lib("gradualizer/include/gradualizer.hrl").

-if(?OTP_RELEASE >= 24).
format_exception(Kind, Reason, StackTrace) ->
    erl_error:format_exception(Kind, Reason, StackTrace).
-else.
format_exception(Kind, Reason, StackTrace) ->
    io_lib:format("~p:~p ~p", [Kind, Reason, StackTrace]).
-endif.

-spec format_binary_string(io:format(), [term()]) -> binary().
format_binary_string(Format, Data) ->
    ?assert_type(unicode:characters_to_binary(io_lib:format(Format, Data)), binary()).

-spec format_binary_string(io:format(), [term()], [{chars_limit, integer()}]) -> binary().
format_binary_string(Format, Data, Options) ->
    ?assert_type(unicode:characters_to_binary(io_lib:format(Format, Data, Options)), binary()).

-spec assert_to_binary(unicode:chardata()) -> binary().
assert_to_binary(String) ->
    ?assert_type(unicode:characters_to_binary(String), binary()).
