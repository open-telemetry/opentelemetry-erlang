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
%% @private
%%%-----------------------------------------------------------------------
-module(otel_utils).

-export([format_exception/3,
         format_binary_string/2,
         format_binary_string/3,
         assert_to_binary/1,
         unicode_to_binary/1]).

-if(?OTP_RELEASE >= 24).
format_exception(Kind, Reason, StackTrace) ->
    erl_error:format_exception(Kind, Reason, StackTrace).
-else.
format_exception(Kind, Reason, StackTrace) ->
    io_lib:format("~p:~p ~p", [Kind, Reason, StackTrace]).
-endif.

-spec format_binary_string(io:format(), [term()]) -> {ok, binary()} | {error, bad_binary_conversion}.
format_binary_string(Format, Data) ->
    unicode_to_binary(io_lib:format(Format, Data)).

-spec format_binary_string(io:format(), [term()], [{chars_limit, integer()}]) -> {ok, binary()} | {error, bad_binary_conversion}.
format_binary_string(Format, Data, Options) ->
    unicode_to_binary(io_lib:format(Format, Data, Options)).

-spec assert_to_binary(unicode:chardata()) -> binary().
assert_to_binary(String) ->
    case unicode:characters_to_binary(String) of
        Bin when is_binary(Bin) ->
            Bin;
        _ ->
            error({bad_binary_conversion, String})
    end.

-spec unicode_to_binary(unicode:chardata()) -> {ok, binary()} | {error, bad_binary_conversion}.
unicode_to_binary(String) ->
    case unicode:characters_to_binary(String) of
        Bin when is_binary(Bin) ->
            {ok, Bin};
        _ ->
            {error, bad_binary_conversion}
    end.
