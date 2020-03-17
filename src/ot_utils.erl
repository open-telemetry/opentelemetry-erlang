-module(ot_utils).

-export([instrumentation_library/2]).

-include("ot_span.hrl").

instrumentation_library(Name, Vsn) ->
    #instrumentation_library{name=name_to_binary(Name),
                             version=vsn_to_binary(Vsn)}.

%% Vsn can't be an atom or anything but a list or binary
%% so return `undefined' if it isn't a list or binary.
vsn_to_binary(Vsn) when is_list(Vsn) ->
    list_to_binary(Vsn);
vsn_to_binary(Vsn) when is_binary(Vsn) ->
    Vsn;
vsn_to_binary(_) ->
    undefined.

%% name can be atom, list or binary. But atom `undefined'
%% must stay as `undefined' atom.
name_to_binary(undefined)->
    undefined;
name_to_binary(T) when is_atom(T) ->
    atom_to_binary(T, utf8);
name_to_binary(T) when is_list(T) ->
    list_to_binary(T);
name_to_binary(T) when is_binary(T) ->
    T.
