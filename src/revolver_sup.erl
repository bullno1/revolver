-module(revolver_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% supervisor

init([]) ->
	%{Name, {Module, StartFunc, Args}, permanent, 5000, supervisor, [Module]}
	ChildSpecs = [revolver_spec()],
	{ok, { {rest_for_one, 5, 60}, ChildSpecs}}.

% private

revolver_spec() ->
	{rev_reloader, {rev_reloader, start_link, []}, permanent, 1000, worker, [rev_reloader]}.
