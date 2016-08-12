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

%% API
-export([router/2]).

router(Dest, Data) ->
    Mod = module,
    erlang:send(Dest, {from_client, {Mod, Data}}).
