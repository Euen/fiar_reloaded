-module(fiar_match).

-behaviour(gen_server).

-type action()       :: play.
-type error_reason() :: wrong_player | term().
-type state()        :: #{match => t()}.
-type user_id()      :: fiar_user:id().

-opaque t()       :: fiar_core:match().
-opaque id()      :: fiar_core:match_id().
-opaque col_num() :: fiar_core:col_num().
-opaque reply()   :: {fiar_core:result(), t()} | {error, error_reason()} | term().

-export_type([t/0, id/0, col_num/0, reply/0]).

%% API
-export([create/2, start_link/3, match_spec/0, play/3]).
-export([fetch/1, current_matches/1, to_view/1]).
-export([get/2, set/3]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%%====================================================================
%% API
%%====================================================================

-spec create(user_id(), user_id()) -> id().
create(UserId1, UserId2) ->
  MatchId = fiar_core:match_id(UserId1, UserId2),
  case fiar_sup:start_child(MatchId, UserId1, UserId2) of
    {ok, _Pid} ->
      MatchId;
    {error, Error} ->
      _ = erlang:display({error, Error}),
      %% @TODO: add lagger messaage
      MatchId
  end.

-spec start_link(id(), user_id(), user_id()) -> {ok, pid()} | {error, term()}.
start_link(MatchId, UserId1, UserId2) ->
  %% change the binary to atom for an ets table
  Ars = {MatchId, UserId1, UserId2},
  gen_server:start_link({local, MatchId}, ?MODULE, Ars, []).

-spec match_spec() -> map().
match_spec() ->
  #{
    id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => temporary, %% cambiar a transient  y que se recupere frente a un fallo inesperado
    shutdown => brutal_kill, %% cambiar a 5000 tiempo para que muera y pueda guardar el match
    type     => worker
  }.

-spec play(id(), col_num(), user_id()) -> reply().
play(Id, Col, UserId) ->
  %% TODO check if I have to parse this responses before to return
  %% TODO What happend if the match does not exists
  gen_server:call(Id, {play, Col, UserId}).

-spec fetch(id()) -> t().
fetch(Id) ->
  gen_server:call(Id, get_match).

-spec current_matches(user_id()) -> [t()] | [].
current_matches(UserId) ->
  lists:foldl(fun({MatchId, _, _, _}, Matches) ->
    %% TODO check if it's possible to put this in just one query
    [fetch(MatchId) | Matches]
  end, [], fiar_match_reference:current_matches(UserId)).

-spec to_view([t()] | t()) -> [map()] | map().
to_view(Matches) when is_list(Matches) ->
  [to_view(Match) || Match <- Matches];
to_view(Match) ->
  fiar_core:match_to_map(Match).

%%====================================================================
%% Getters and Setters
%%====================================================================

-spec get(atom(), t()) -> term().
get(Field, Match) ->
  maps:get(Field, Match).

-spec set(atom(), term(), t()) -> t().
set(Field, Value, Match) ->
  Match#{Field => Value}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init({id(), user_id(), user_id()}) -> {ok, state()} | {stop, already_started}.
init({MatchId, UserId1, UserId2}) ->
  case fiar_match_reference:insert(MatchId, UserId1, UserId2, self()) of
    true ->
      Match = fiar_core:new_match(MatchId, UserId1, UserId2),
      {ok, #{match => Match}};
    false ->
      {stop, already_started}
  end.

-spec handle_call({action(), col_num(), user_id()}, {pid(), term()}, state()) ->
  {reply, reply(), state()} | {stop, normal, reply(), state()}.
handle_call(get_match, _From, #{match := Match} = State) ->
  {reply, Match, State};
handle_call({play, Col, UserId}, _From, #{match := Match} = State) ->
  try is_player_turn(Match, UserId) andalso fiar_core:play(Col, Match) of
    false ->
      {reply, {error, wrong_player}, State};
    {next, NewMatch} ->
      {reply, {next, NewMatch}, State#{match => NewMatch}};
    {FinalResult, NewMatch} ->
      %% TODO save the results in the handle terminate
      {stop, normal, {FinalResult, NewMatch}, State#{match => NewMatch}}
  catch
    _:Reason ->
      {reply, {error, Reason}, State}
  end;
handle_call(_, _From, State) ->
  {reply, unknown_message, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Req, State) ->
  {noreply, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_player_turn(t(), user_id()) -> boolean().
is_player_turn(Match, UserId) ->
  case next_player(Match) of
    UserId -> true;
    _      -> false
  end.

-spec next_player(t()) -> user_id().
next_player(Match) ->
  case get(next_chip, Match) of
    1 -> get(player_id1, Match);
    2 -> get(player_id2, Match)
  end.