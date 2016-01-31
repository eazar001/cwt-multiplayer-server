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


%%================================================================================
%% Interface
%%================================================================================


start() ->
  io:format("Initializing server...~n"),
  gen_server:start({local, ?MODULE}, ?MODULE, ?INITTABLES, []),
  io:format("Server started.~n").


stop() ->
  io:format("Stopping server...~n"),
  gen_server:cast(?MODULE, stop),
  io:format("Server stopped.~n").


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


%%================================================================================
%% Callback functions
%%================================================================================


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

handle_call({ join_game, { Uname, Pos, Game } }, _From, Ts) ->
  #game{name=Gname} = Game,
  #tables{players=Ps} = Ts,
  case valid_player(Uname, {Game, Pos}, Ts) of
    true ->
      Player = #player{name=Uname, game=Gname, position=Pos, status=active},
      Update = lists:map(fun(P) -> join_state(P, {Uname, Gname, Pos}) end, Ps),
      case Update of
        Ps -> { reply, ?OK, Ts#tables{players=[Player|Ps]} };
        _ -> { reply, ?OK, Ts#tables{players=Update} }
      end;

    _ -> {reply, ?FAIL, Ts}
  end;

handle_call({ resign_game, { Uname, Pos, Game } }, _From, Ts) ->
  #tables{players=Ps} = Ts,
  #game{name=Gname} = Game,
  case active_player_in_game({Uname, Pos}, Gname, Ts) of
    [PlayerProfile] ->
      PlayerUpdate = lists:map(fun(P) -> resign_state(P, PlayerProfile) end, Ps),
      { reply, ?OK, Ts#tables{players=PlayerUpdate} };

    _ -> {reply, ?FAIL, Ts}
  end;

handle_call({add_user, U}, _From, Ts=#tables{users=Us}) ->
  case lists:member(U, Us) of
    true -> {reply, ?FAIL, Ts};
    _ -> {reply, ?OK, Ts#tables{users=[U|Us]}}
  end;

handle_call({remove_user, U}, From, Ts=#tables{users=Us}) ->
  reply:remove_user(From, U, Us),
  {noreply, Ts};

handle_call({ping, U}, From, Ts=#tables{users=Us}) ->
  reply:ping(From, U, Us),
  {noreply, Ts}.


%%%%%%%%%%%%%%%%%%
%% Asynchronous %%
%%%%%%%%%%%%%%%%%%


handle_cast(list_users, Ts=#tables{users=[]}) ->
  io:format("No registered users~n"),
  {noreply, Ts};

handle_cast(list_users, Ts=#tables{users=Us}) ->
  [ io:format("~s~n", [U#user.name]) || U <- Us ],
  {noreply, Ts};

handle_cast(list_games, Ts=#tables{games=[]}) ->
  io:format("No registered games~n"),
  {noreply, Ts};

handle_cast(list_games, Ts=#tables{games=Gs}) ->
  [ io:format("~s~n", [G#game.name]) || G <- Gs ],
  {noreply, Ts};

handle_cast(list_players, Ts=#tables{players=[]}) ->
  io:format("No registered players~n"),
  {noreply, Ts};

handle_cast(list_players, Ts=#tables{players=Ps}) ->
  [ io:format("~p~nPOS:~p~nSTATUS:~p~n", [ Name, Pos, Status ])
   || #player{name=Name, position=Pos, status=Status} <- Ps ],
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

handle_info({From, ping_ok}, Ts) ->
  gen_server:reply(From, ?OK),
  {noreply, Ts};

handle_info({From, ping_fail}, Ts) ->
  gen_server:reply(From, ?FAIL),
  {noreply, Ts};

handle_info({From, remove_user_ok, Users}, Ts) ->
  gen_server:reply(From, ?OK),
  {noreply, Ts#tables{users=Users}};

handle_info({From, remove_user_fail, _Users}, Ts) ->
  gen_server:reply(From, ?FAIL),
  {noreply, Ts}.

code_change(_Old, State, _Extra) -> {ok, State}.

terminate(Reason, State) -> {ok, Reason, State}.


%%================================================================================
%% System/Validation
%%================================================================================


%% valid_game(Game, Tables) -> boolean()
%% This function attempts to validate the constraints of a request to register
%% a new game in the system.
valid_game({Game, Pos, Len}, Ts) when Pos > 0, Pos =< Len ->
  #game{limit=Limit,host=Host,name=Name} = Game,
  #tables{users=Us, games=Gs} = Ts,
  valid_game_length(Len, Limit) andalso
  current_user(Host, Us) andalso not current_game(Name, Gs);

valid_game(_, _) -> false.

%% valid_game_length(Length, Limit) -> boolean()
valid_game_length(Length, Length) when Length > 0, Length < 5 -> true;
valid_game_length(_, _) -> false.


%% valid_player(User, {Game, Pos}, Tables) -> boolean()
%% This function attempts to valid the constraints of a request to join a user
%% as a player to an existing game in the system. The request should include the
%% player's username, desired position, and proposed game (its name) to join.
%% If the user is currently in game, then the status must be inactive in order to
%% be considered a candidate for validation.
valid_player(UserName, {Game, Pos}, Ts) ->
  #game{name=GameName, limit=Limit} = Game,
  #tables{users=Us, players=Ps, games=Gs} = Ts,
  current_user(UserName, Us) andalso
  current_game(GameName, Gs) andalso
  validate_player(UserName, GameName, Game, Ts) andalso
  validate_position(Pos, GameName, Limit, Ps).

validate_position(Pos, Gname, Limit, Players) when Pos > 0, Pos =< Limit ->
  not lists:member(Pos, positions_in_game(Gname, Players));

validate_position(_, _, _, _) -> false.

validate_player(Uname, Gname, Game, Ts) ->
  not player_in_game(Uname, Gname, Ts) andalso
  case current_player(Uname, Ts) of
    false -> true;
    Xs -> in_game(Xs, Game)
  end.


%% current_game(GameName, Games) -> boolean()
%% True if system Tables contains a game with the title represented by GameName.
current_game(GameName, Games) ->
  lists:keymember(GameName, #game.name, Games).


%% find_game(GameName, Games) -> false | Tuple
%% Finds the first occurrence of the game and returns the tuple. False otherwise.
find_game(GameName, Games) ->
  lists:keyfind(GameName, #game.name, Games).


%% find_player(UserName, Players) -> false | tuple
%% Find a player in the registered players table and return the entire player
%% profile. Returns false if no match is found.
find_player(UserName, Players) ->
  lists:keyfind(UserName, #player.name, Players).


%% current_user(UserName, Users) -> boolean()
%% True if system Tables contains a user with the name represented by Username.
current_user(UserName, Users) ->
  lists:keymember(UserName, #user.name, Users).


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
active_player_in_game({Uname, Pos}, Gname, #tables{players=Ps}) ->
  Keys = {Uname, Gname, Pos},
  lists:filter(fun(Player) -> active_match(Player, Keys) end, Ps).

active_match(#player{name=Un,game=Gn,position=P,status=active}, {Un, Gn, P}) ->
  true;

active_match(_, _) -> false.


%% player_in_game(string(), string(), Tuple) -> boolean()
%% Same as active_player_in_game/3, but returns true regardless of status or
%% position.
player_in_game(UserName, GameName, #tables{players=Ps}) ->
  Keys = {UserName, GameName},
  [] =/= lists:filter(fun(Player) -> in_game_match(Player, Keys) end, Ps).

in_game_match(#player{name=Uname, game=Gname}, {Uname, Gname}) -> true;
in_game_match(_, _) -> false.


%% positions_in_game(GameName, Players) -> [integer()]
%% Takes a GameName string and list of Player profile records and filters the list
%% of records to show only the integer representations of the active players that
%% are partaking in the game.
positions_in_game(GameName, Players) ->
  Ps = lists:filter(fun(P) -> pos_match(P, GameName) end, Players),
  lists:map(fun(P) -> P#player.position end, Ps).

pos_match(#player{status=active, game=GameName}, GameName) -> true;
pos_match(_, _) -> false.


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


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions %%
%%%%%%%%%%%%%%%%%%%%%%%%


resign_state(MatchingPlayer, MatchingPlayer) ->
  MatchingPlayer#player{status=inactive};

resign_state(Player, _) -> Player.


join_state(#player{name=Uname, game=Gname}, {Uname, Gname, Pos}) ->
  #player{name=Uname, game=Gname, position=Pos, status=active};

join_state(Player, _) -> Player.


bool_to_answer(true) -> yes;
bool_to_answer(false) -> no.
