%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
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
-module(ot_tracer_provider).

-behaviour(gen_server).

-export([start_link/2,
         resource/0,
         register_application_tracer/1,
         register_tracer/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3]).

-type cb_state() :: term().

-callback init(term()) -> {ok, cb_state()}.
-callback register_tracer(atom(), string(), cb_state()) -> boolean().
-callback resource(cb_state()) -> term() | undefined.

-record(state, {callback :: module(),
                cb_state :: term()}).

start_link(ProviderModule, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ProviderModule, Opts], []).

-spec resource() -> term() | undefined.
resource() ->
    try
        gen_server:call(?MODULE, resource)
    catch exit:{noproc, _} ->
            %% ignore because no SDK has been included and started
            undefined
    end.


-spec register_tracer(atom(), string()) -> boolean().
register_tracer(Name, Vsn) ->
    try
        gen_server:call(?MODULE, {register_tracer, Name, Vsn})
    catch exit:{noproc, _} ->
            %% ignore register_tracer because no SDK has been included and started
            false
    end.

-spec register_application_tracer(atom()) -> boolean().
register_application_tracer(Name) ->
    try
        {ok, Vsn} = application:get_key(Name, vsn),
        {ok, Modules} = application:get_key(Name, modules),
        gen_server:call(?MODULE, {register_tracer, Name, Vsn, Modules})
    catch exit:{noproc, _} ->
            %% ignore register_tracer because no SDK has been included and started
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

handle_call({register_tracer, Name, Vsn, Modules}, _From, State=#state{callback=Cb,
                                                                       cb_state=CbState}) ->
    _ = Cb:register_tracer(Name, Vsn, Modules, CbState),
    {reply, true, State};
handle_call({register_tracer, Name, Vsn}, _From, State=#state{callback=Cb,
                                                              cb_state=CbState}) ->
    _ = Cb:register_tracer(Name, Vsn, CbState),
    {reply, true, State};
handle_call(resource, _From, State=#state{callback=Cb,
                                          cb_state=CbState}) ->
    Resource = Cb:resource(CbState),
    {reply, Resource, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% TODO: Use `Extra' as options to update the state like the sampler?
code_change(_OldVsn, State=#state{callback=Cb,
                                  cb_state=CbState}, _Extra) ->
    NewCbState = Cb:code_change(CbState),
    {ok, State#state{cb_state=NewCbState}}.

%%
