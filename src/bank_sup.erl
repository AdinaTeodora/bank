%%%-------------------------------------------------------------------
%% @doc bank top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bank_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(BANK_SERVER, bank_server).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy  => one_for_one,
    intensity => 100,
    period    => 5
  },

  ChildSpecs = [#{
    id       => ?BANK_SERVER,
    start    => {?BANK_SERVER, start_link, []},
    restart  => permanent,
    shutdown => 2000,
    type     => worker,
    modules  => [?BANK_SERVER]
  }],

  {ok, {SupFlags, ChildSpecs}}.
