-module(db).

%%% Callback
-export(
   [ init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3 ]).

%%% API
-export(
   [ start/0
    ,create_game/2
    ,list_users/0
    ,list_games/0
    ,list_players/0
    ,current_user/1
    ,current_game/1
    ,current_game/2
    ,join_game/3
    ,resign_game/3
    ,find_game/2
    ,find_player/2
    ,active_game/1
    ,login/1
    ,ping/1
    ,logout/1
    ,stop/0 ]).

-behavior(gen_server).

-define(OK, "{ status:ok }").
-define(FAIL, "{ status:fail }").

-define(INITTABLES, #tables{}). % [users, players, games]

-record(tables, { users=[]
                 ,players=[]
                 ,games=[] }).

-record(user, { name }).   % name -> string()

-record(game, { name       % name -> string()
               ,host       % host -> string()
               ,limit      % limit -> integer()
               ,layout }). % layout -> [atom()]

-record(player, { name       % name -> string()
                 ,game       % game -> string()
                 ,position   % position -> integer()
                 ,status }). % status -> atom()


%%--------------------------------------------------------------------------------
%% Interface
%%--------------------------------------------------------------------------------


start() ->
  io:format("Initializing server...~n"),
  gen_server:start({local, ?MODULE}, ?MODULE, ?INITTABLES, []),
  io:format("Server started.~n").


%% list_users() -> ok
%% List all the current users that are registered in the server's database.
list_users() ->
  gen_server:cast(?MODULE, list_users).


%% list_games() -> ok
%% List all the current games that are registered in the server.
list_games() ->
  gen_server:cast(?MODULE, list_games).


%% list_players() -> ok
%% List all the current players that are registered in the server.
list_players() ->
  gen_server:cast(?MODULE, list_players).


%% current_user() -> ok
%% Print whether or not user is registered in the system.
current_user(UserName) ->
  gen_server:cast(?MODULE, {current_user, UserName}).


%% current_game() -> ok
%% Print whether or not game is registered in the system.
current_game(GameName) ->
  gen_server:cast(?MODULE, {current_game, GameName}).


%% login(string()) -> string()
%% User attempts to log in to the game. The user will be logged in and registered
%% as a user only if the requested username does not already exist.
login(Name) ->
  gen_server:call(?MODULE, { add_user, #user{name=Name} }).


%% ping(string()) -> string()
%% Send a ping request to keep the user connection alive. Successful only if the
%% user currently exists in the database.
ping(Name) ->
  gen_server:call(?MODULE, { ping, #user{name=Name} }).


%% create_game(Tuple, integer()) -> string()
%% Create a game in the game server database. Successful only if There is no
%% game of the current title in the database and the total number of players is
%% within the specified limit.
create_game(Game, Position) ->
  gen_server:call(?MODULE, { create_game, {Game, Position} }).


%% join_game(string(), integer(), Tuple) -> string()
%% Join a user to a desired position in a game that's already been created.
join_game(UserName, Position, Game) ->
  gen_server:call(?MODULE, { join_game, {UserName, Position, Game} }).


%% resign_game(string(), integer(), Tuple) -> string()
%% Resign an active user in a currently registered game with the "inactive"
%% status. The UserName must be registered as a current and active player under
%% the profile of the specified Game record.
resign_game(UserName, Position, Game) ->
  gen_server:call(?MODULE, { resign_game, {UserName, Position, Game} }).


%% User attempts to log out of the game's system. The user will be unregistered
%% if there is a match for the provided username in the database.
logout(Name) ->
  gen_server:call(?MODULE, { remove_user, #user{name=Name} }).


stop() ->
  gen_server:cast(?MODULE, stop).


%%--------------------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%
%% Synchronous %%
%%%%%%%%%%%%%%%%%


init(State) -> {ok, State}.


handle_call({ create_game, {Game, Position} }, _From, Ts=#tables{games=Gs}) ->
  Length = length(Game#game.layout),
  case valid_game({Game, Position, Length}, Ts) of
    true  ->
      {reply, ?OK, Ts#tables{games=[Game|Gs]}};
    _ ->
      {reply, ?FAIL, Ts}
  end;

handle_call({ join_game, { Uname, Pos, Game=#game{name=Gname} } }, _From,
            Ts=#tables{players=Ps}) ->

  case valid_player(Uname, {Game, Pos}, Ts) of
    true ->
      Player = #player{name=Uname, game=Gname, position=Pos, status=active},
      PlayerUpdate =
        lists:map(fun(P) ->
                    if
                      P#player.name =:= Uname andalso
                      P#player.game =:= Gname ->
                        Player;
                      true -> P
                    end
                  end, Ps),
      case PlayerUpdate of
        Ps -> { reply, ?OK, Ts#tables{players=[Player|Ps]} };
        _ -> { reply, ?OK, Ts#tables{players=PlayerUpdate} }
      end;

    _ -> {reply, ?FAIL, Ts}
  end;

handle_call({ resign_game, { Uname, Pos, #game{name=Gname} } }, _From,
            Ts=#tables{players=Ps}) ->

  case active_player_in_game({Uname, Pos}, Gname, Ts) of
    [PlayerProfile] ->
      PlayerUpdate =
        lists:map(fun(P) ->
                    case P of
                      PlayerProfile ->
                        P#player{status=inactive};
                      _ -> P
                    end
                  end, Ps),
      { reply, ?OK, Ts#tables{players=PlayerUpdate} };

    _ -> {reply, ?FAIL, Ts}
  end;

handle_call({add_user, U}, _From, Ts=#tables{users=Us}) ->
  case lists:member(U, Us) of
    true -> {reply, ?FAIL, Ts};
    _ -> {reply, ?OK, Ts#tables{users=[U|Us]}}
  end;

handle_call({remove_user, U}, _From, Ts=#tables{users=Us}) ->
  NewUsers = lists:delete(U, Us),
  NewTables = Ts#tables{users=NewUsers},
  case NewUsers of
    Us -> {reply, ?FAIL, NewTables};
    _ -> {reply, ?OK, NewTables}
  end;

handle_call({ping, U}, _From, Ts=#tables{users=Us}) ->
  case lists:member(U, Us) of
    true -> {reply, ?OK, Ts};
    _ -> {reply, ?FAIL, Ts}
  end.


%%%%%%%%%%%%%%%%%%
%% Asynchronous %%
%%%%%%%%%%%%%%%%%%


handle_cast(list_users, Ts) when Ts#tables.users =:= [] ->
  io:format("No registered users~n"),
  {noreply, Ts};

handle_cast(list_users, Ts=#tables{users=Us}) ->
  [ io:format("~s~n", [U#user.name]) || U <- Us ],
  {noreply, Ts};

handle_cast(list_games, Ts=#tables{games=Gs}) when Gs =:= [] ->
  io:format("No registered games~n"),
  {noreply, Ts};

handle_cast(list_games, Ts=#tables{games=Gs}) ->
  [ io:format("~s~n", [G#game.name]) || G <- Gs ],
  {noreply, Ts};

handle_cast(list_players, Ts) when Ts#tables.players =:= [] ->
  io:format("No registered players~n"),
  {noreply, Ts};

handle_cast(list_players, Ts=#tables{players=Ps}) ->
  [ io:format("~p~nPOS:~p~nSTATUS:~p~n",
              [P#player.name, P#player.position, P#player.status]) || P <- Ps ],
  {noreply, Ts};

handle_cast({current_user, UserName}, Ts=#tables{users=Us}) ->
  Present = lists:keymember(UserName, #user.name, Us),
  io:format("~p~n", [bool_to_answer(Present)]),
  {noreply, Ts};

handle_cast({current_game, GameName}, Ts=#tables{games=Gs}) ->
  Present = lists:keymember(GameName, #game.name, Gs),
  io:format("~p~n", [bool_to_answer(Present)]),
  {noreply, Ts};

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.

terminate(Reason, State) -> {ok, Reason, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions %%
%%%%%%%%%%%%%%%%%%%%%%%%


%% valid_game(Game, Tables) -> boolean()
%% This function attempts to validate the constraints of a request to register
%% a new game in the system.
valid_game({#game{limit=Limit,layout=Layout,host=Host,name=Name}, Pos, Len},
           #tables{users=Us,games=Gs}) when Limit =:= Len,
                                            Pos > 0,
                                            Pos =< Len ->
  current_user(Host, Us) andalso
  not current_game(Name, Gs) andalso
  Limit > 0 andalso Limit < 5 andalso
  length(Layout) =:= Limit;

valid_game(_, _) -> false.


%% valid_player(User, {Game, Pos}, Tables) -> boolean()
%% This function attempts to valid the constraints of a request to join a user
%% as a player to an existing game in the system. The request should include the
%% player's username, desired position, and proposed game (its name) to join.
%% If the user is currently in game, then the status must be inactive in order to
%% be considered a candidate for validation.
valid_player(UserName,
             {Game=#game{name=GameName, limit=Limit}, Pos},
             Ts=#tables{users=Us, players=Ps, games=Gs}) ->
  current_user(UserName, Us) andalso
  current_game(GameName, Gs) andalso
  not player_in_game(UserName, GameName, Ts) andalso
  Pos > 0 andalso
  Pos =< Limit andalso
  case current_player(UserName, Ts) of
    false -> true;
    Xs -> in_game(Xs, Game)
  end andalso
  not lists:member(Pos, positions_in_game(GameName, Ps)).


%% current_game(GameName, Tables) -> boolean()
%% True if system Tables contains a game with the title represented by GameName.
current_game(GameName, GameTable) ->
  lists:keymember(GameName, #game.name, GameTable).


%% find_game(GameName, Tables) -> false | Tuple
%% Finds the first occurrence of the game and returns the tuple. False otherwise.
find_game(GameName, GameTable) ->
  lists:keyfind(GameName, #game.name, GameTable).


%% find_player(UserName, PlayerTable) -> false | tuple
%% Find a player in the registered players table and return the entire player
%% profile. Returns false if no match is found.
find_player(UserName, PlayerTable) ->
  lists:keyfind(UserName, #player.name, PlayerTable).

%% current_user(UserName, UserTable) -> boolean()
%% True if system Tables contains a user with the name represented by Username.
current_user(UserName, UserTable) ->
  lists:keymember(UserName, #user.name, UserTable).


%% current_player(UserName, Players) -> false | [Tuple]
%% Returns a list of registered in-game players that match the UserName.
current_player(UserName, #tables{players=Ps}) ->
  case lists:filter(fun(P) -> P#player.name =:= UserName end, Ps) of
    [] -> false;
    Xs -> Xs
  end.


%% active_player_in_game(Tuple, string(), Tuple) -> boolean()
%% Takes a UserName and Position, and returns a list of all player profiles that
%% represents an active player profile in the game specified by GameName.
active_player_in_game({UserName, Pos}, GameName, #tables{players=Ps}) ->
  lists:filter(fun(#player{name=N, game=G, position=P, status=S}) ->
               UserName =:= N andalso GameName =:= G andalso
               Pos =:= P andalso S =:= active end, Ps).


%% player_in_game(string(), string(), Tuple) -> boolean()
%% Same as active_player_in_game/3, but returns true regardless of status or
%% position.
player_in_game(UserName, GameName, #tables{players=Ps}) ->
  L = lists:filter(fun(#player{name=N, game=G}) ->
                   UserName =:= N andalso GameName =:= G end, Ps),
  L =/= [].

%% positions_in_game(GameName, Players) -> [integer()]
%% Takes a GameName string and list of Player profile records and filters the list
%% of records to show only the integer representations of the active players that
%% are partaking in the game.
positions_in_game(GameName, Players) ->
  Ps = lists:filter(fun(P) -> P#player.game =:= GameName andalso
                    P#player.status =:= active end, Players),
  lists:map(fun(P) -> P#player.position end, Ps).


%% in_game(Players, Game) -> boolean()
%% Takes a list of Player profile records, and examines them to see if any of them
%% are currently playing the Game specified in the input argument.
in_game(Players, #game{name=N}) ->
  lists:keymember(N, #player.game, Players).


%% active_game(PlayerList) -> boolean()
%% Active game if we find that there is more than one active player comprising at
%% least two games.
active_game([H|Rest]) ->
  lists:any(fun(X) -> X =/= H end, Rest).


bool_to_answer(true) -> yes;
bool_to_answer(false) -> no.
