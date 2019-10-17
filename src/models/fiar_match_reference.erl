-module(fiar_match_reference).

-type match_id()  :: fiar_match:id().
-type user_id()   :: fiar_user:id().
-type match_reference() :: {match_id(), user_id(), user_id(), pid()}.

-export ([new/0, insert/4, fetch/1, current_matches/1]).

%%====================================================================
%% API
%%====================================================================

-spec new() -> {ok, atom()}.
new() ->
  ?MODULE = ets:new(?MODULE, [set, named_table, public]),
  {ok, ?MODULE}.

-spec insert(match_id(), user_id(), user_id(), pid()) -> boolean().
insert(MatchId, UserId1, UserId2, Pid) ->
  ets:insert_new(?MODULE, {MatchId, UserId1, UserId2, Pid}).

-spec fetch(match_id()) -> [match_reference()] | [].
fetch(MatchId) ->
  ets:lookup(?MODULE, MatchId).

-spec current_matches(user_id()) -> [match_reference()] | [].
current_matches(UserId) ->
  Query = [{
    {'$1', '$2', '$3', '$4'},
    [{'orelse', {'==', '$2', UserId}, {'==', '$3', UserId}}],
    ['$_']
  }],
  case ets:select(?MODULE, Query) of
    {error, _Reason} -> [];
    Selection -> Selection
  end.
