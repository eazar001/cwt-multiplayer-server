-module(reply).
-compile(export_all).

-behavior(gen_event).


%%================================================================================
%% Interface
%%================================================================================


start_link(ServerName) ->
  io:format("Initializing reply handler~n"),
  {ok, Pid} = gen_event:start_link({local, ?MODULE}),
  gen_event:add_handler(?MODULE, ?MODULE, whereis(ServerName)),
  io:format("Reply handler started.~n"),
  {ok, Pid}.


ping(From, User, Users) ->
  gen_event:notify(?MODULE, {ping, From, User, Users}).


remove_user(From, User, Users) ->
  gen_event:notify(?MODULE, {remove_user, From, User, Users}).


stop() ->
  io:format("Stopping reply handler...~n"),
  gen_event:stop(?MODULE),
  io:format("Reply handler stopped.~n").


%%================================================================================
%% Callback functions
%%================================================================================


init(ServerName) -> {ok, ServerName}.


handle_event({ping, From, User, Users}, Server) ->
  case lists:member(User, Users) of
    true -> Server ! {From, ping_ok};
    _ -> Server ! {From, ping_fail}
  end,
  {ok, Server};

handle_event({remove_user, From, User, Users}, Server) ->
  NewUsers = lists:delete(User, Users),
  case NewUsers of
    Users -> Server ! {From, remove_user_fail, NewUsers};
    _ -> Server ! {From, remove_user_ok, NewUsers}
  end,
  {ok, Server}.

handle_call(_Msg, State) ->
  {ok, State}.

handle_info(_Msg, Server) ->
  {ok, Server}.

code_change(_Old, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  {ok, Reason, State}.
