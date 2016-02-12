-module(cwt_sup).

%% API
-export([start_link/0]).

%% Callback
-export([init/1]).

-behavior(supervisor).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags =
    #{ strategy => rest_for_one,
       intensity => 1,
       period => 5 },

  ChildSpecs =
    [
     #{ id => server,
        start => {db, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [db] },

     #{ id => reply_handler,
        start => {reply, start_link, [db]},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [dynamic] }
    ],
  {ok, {SupFlags, ChildSpecs}}.
