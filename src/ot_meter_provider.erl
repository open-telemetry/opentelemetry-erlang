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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(ot_meter_provider).

-behaviour(gen_server).

-export([start_link/2,
         register_application_meter/1,
         register_meter/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-type cb_state() :: term().

-callback init(term()) -> {ok, cb_state()}.
-callback register_meter(atom(), string(), cb_state()) -> boolean().

-record(state, {callback :: module(),
                cb_state :: term()}).

start_link(ProviderModule, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ProviderModule, Opts], []).

-spec register_meter(atom(), string()) -> boolean().
register_meter(Name, Vsn) ->
    try
        gen_server:call(?MODULE, {register_meter, Name, Vsn})
    catch exit:{noproc, _} ->
            %% ignore register_meter because no SDK has been included and started
            false
    end.

-spec register_application_meter(atom()) -> boolean().
register_application_meter(Name) ->
    try
        {ok, Vsn} = application:get_key(Name, vsn),
        {ok, Modules} = application:get_key(Name, modules),
        gen_server:call(?MODULE, {register_meter, Name, Vsn, Modules})
    catch exit:{noproc, _} ->
            %% ignore register_meter because no SDK has been included and started
            false
    end.

init([ProviderModule, Opts]) ->
    case ProviderModule:init(Opts) of
        {ok, CbState} ->
            {ok, #state{callback=ProviderModule,
                        cb_state=CbState}};
        Other ->
            Other
    end.

handle_call({register_meter, Name, Vsn, Modules}, _From, State=#state{callback=Cb,
                                                                      cb_state=CbState}) ->
    _ = Cb:register_meter(Name, Vsn, Modules, CbState),
    {reply, true, State};
handle_call({register_meter, Name, Vsn}, _From, State=#state{callback=Cb,
                                                             cb_state=CbState}) ->
    _ = Cb:register_meter(Name, Vsn, CbState),
    {reply, true, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
