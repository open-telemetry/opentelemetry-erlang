%%%-------------------------------------------------------------------
%% @doc opentelemetry_experimental top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_experimental_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([_Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    %%
    MetricSup = #{id => otel_metrics_sup,
                  start => {otel_metrics_sup, start_link, []},
                  restart => permanent,
                  shutdown => infinity,
                  type => supervisor,
                  modules => [otel_metrics_sup]},

    ChildSpecs = [MetricSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
