%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 内部工具集
%%% @end
%%% Created : 12. 八月 2016 14:22
%%%-------------------------------------------------------------------
-module(cgateway_util).
-author("calvin").

%% API
-export([gateway_id/1]).

%% @doc 生产gateway_listen_server的进程ID
-spec gateway_id(term()) -> atom().
gateway_id(ID) ->
    erlang:list_to_atom(lists:concat(["cgw_", ID])).