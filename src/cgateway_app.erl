%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 网关服务公用接口
%%% @end
%%% Created : 12. 八月 2016 13:42
%%%-------------------------------------------------------------------
-module(cgateway_app).
-author("calvin").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cgateway_sup:start_link().

stop(_State) ->
    ok.
