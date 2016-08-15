%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 路由消息协议
%%% @end
%%% Created : 12. 八月 2016 17:16
%%%-------------------------------------------------------------------
-module(cgateway_router).
-author("calvin").
-include("cgateway.hrl").
%% API
-export([router/2]).

%% @doc ID还未初始化，表示不用路由的消息
router(ID, Data) when ID =:= 0 ->
    Dest = none,
    Mod = module,
    erlang:send(Dest, {from_client, {Mod, Data}});

%% @doc ID已经初始化完，需要根据消息协议路由消息
router(_ID, Data)->
    Dest = none,
    Mod = module,
    erlang:send(Dest, {from_client, {Mod, Data}}).

