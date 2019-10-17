-module(fiart_user_handler_SUITE).
-author('euenlopez@gmail.com').

-include_lib("common_test/include/ct.hrl").
-include_lib("mixer/include/mixer.hrl").
-mixin([{fiart_ct, [init_per_suite/1, end_per_suite/1]}]).

%% CT
-export([all/0]).

%% Test Cases
-export([
  create/1,
  validation_errors/1
]).

-define(PATH, <<"/users">>).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
  [
    create,
    validation_errors
  ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

create(Config) ->
  Body = #{username => <<"euen">>, pass => <<"123456">>},

  {200, #{<<"username">> := <<"euen">>}} = fiart_http:post(?PATH, Body, Config).

validation_errors(Config) ->
  Bodies = [
    #{},
    #{username => <<"jhon">>},
    #{username => <<"j@">>, pass => <<"123456">>},
    #{username => <<"SuperLongNameSuperLongNameSuperLongNameSuperLongName">>, pass => <<"123456">>},
    #{pass => <<"123456">>},
    #{username => <<"jhon">>, pass => <<"@1">>},
    #{username => <<"jhon">>, pass => <<"123456789123456789123456789123456789123456789123456">>}
  ],

  lists:foreach(fun(Body) ->
    {422, _} = fiart_http:post(?PATH, Body, Config)
  end, Bodies).