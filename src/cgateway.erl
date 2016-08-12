%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 网关服务公用接口
%%% @end
%%% Created : 12. 八月 2016 13:42
%%%-------------------------------------------------------------------
-module(cgateway).
-author("calvin").

%% API
-export([start/0,
    stop/0]).

start() ->
    application:start(cgateway).

stop() ->
    application:stop(cgateway).
