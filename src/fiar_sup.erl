%%%-------------------------------------------------------------------
%% @doc fiar top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fiar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(fiar_match:id(), fiar_user:id(), fiar_user:id()) ->
  {ok, supervisor:child()} | {error, supervisor:startchild_err()}.
start_child(MatchId, UserId1, UserId2) ->
  supervisor:start_child(?MODULE, [MatchId, UserId1, UserId2]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  {ok, fiar_match_reference} = fiar_match_reference:new(),
  SupFlags = #{
    strategy  => simple_one_for_one,
    intensity => 0,
    period    => 1
  },
  {ok, {SupFlags, [fiar_match:match_spec()]}}.

%%====================================================================
%% Internal functions
%%====================================================================
