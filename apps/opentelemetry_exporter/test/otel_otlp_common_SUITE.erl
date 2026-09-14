-module(otel_otlp_common_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() ->
    [
        to_attrs_list_atoms,
        to_attrs_list_binaries,
        to_attrs_tuple_binaries
    ].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

to_attrs_list_atoms(_Config) ->
    ArrayExpected = #{value => {array_value, #{values => [#{value => {string_value,<<"a">>}}, #{value => {string_value,<<"b">>}}]}}},
    [#{value := Actual}] = otel_otlp_common:to_attributes(#{atoms => [a, b]}),
    ?assertEqual(ArrayExpected, Actual).


to_attrs_list_binaries(_Config) ->
    ArrayExpected = #{value => {array_value, #{values => [#{value => {string_value,<<"a">>}}, #{value => {string_value,<<"b">>}}]}}},
    [#{value := Actual}] = otel_otlp_common:to_attributes(#{atoms => [<<"a">>, <<"b">>]}),
    ?assertEqual(ArrayExpected, Actual).


to_attrs_tuple_binaries(_Config) ->
    ArrayExpected = #{value => {array_value, #{values => [#{value => {string_value,<<"a">>}}, #{value => {string_value,<<"b">>}}]}}},
    [#{value := Actual}] = otel_otlp_common:to_attributes(#{atoms => {<<"a">>, <<"b">>}}),
    ?assertEqual(ArrayExpected, Actual).
