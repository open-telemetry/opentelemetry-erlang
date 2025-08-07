-module(otel_file_configuration).

-export([parse_file/1,
         parse_binary/1]).

%%
-spec parse_file(file:filename_all()) -> otel_configuration:t().
parse_file(JsonFile) ->
    {ok, File} = file:read_file(JsonFile),
    parse_binary(File).

%% TODO: Log a warning on failure to parse and return empty configuration
-spec parse_binary(binary()) -> otel_configuration:t().
parse_binary(Bin) ->
    Push = fun(Key, Value, Acc) -> [{binary_to_atom(Key), Value} | Acc] end,
    {Json, ok, <<>>} = json:decode(Bin, ok, #{object_push => Push, null => undefined}),

    Json.
