-module(fiar_auth).
-author('euen@inakanetworks.com').

-export([
         check_auth/1,
         current_user/1,
         credentials/1
        ]).

-define(AUTH_HEADER, <<"Basic realm=\"FiaR\"">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_auth(Req) ->
  case current_user(Req) of
    undefined ->
      {not_authenticated, ?AUTH_HEADER, Req};
    notfound ->
      {not_authenticated, ?AUTH_HEADER, Req};
    User ->
      {authenticated, User, Req}
  end.

current_user(Req) ->
  case credentials(Req) of
    undefined ->
      undefined;
    #{username := Username} ->
    %% @TODO: check if the find is needed or I can just save the map, maybe cash this
      fiar_user_repo:find_by_username(Username)
  end.

credentials(Req) ->
  try cowboy_req:parse_header(<<"authorization">>, Req) of
    {basic, Username, Password} ->
      #{username => Username, password => Password};
    undefined ->
        case cowboy_req:parse_cookies(Req) of
          [] -> undefined;
          [{<<"token">>, Token}, _, _, _] ->
            parse_cookie_auth(Token)
        end
  catch
    _:Exception:Stacktrace ->
      ErrorMsg = "error trying to check auth: ~p~n\tStack: ~p~n",
      lager:warning(ErrorMsg, [Exception, Stacktrace]),
      undefined
  end.

parse_cookie_auth(Token) ->
  try base64:decode(Token) of
    UserPass ->
      case binary:split(UserPass, <<":">>) of
        [Username, Password] -> #{username => Username, password => Password};
        _ -> undefined
      end
  catch
    _:_ -> undefined
  end.
