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
-export([gateway_id/1,
    send_from_server/2,
    send_ready_socket/1,
    send_to_client/2]).

%% @doc 生产gateway_listen_server的进程ID
-spec gateway_id(term()) -> atom().
gateway_id(ID) ->
    erlang:list_to_atom(lists:concat(["cgw_", ID])).

send_from_server(Dest, Data) ->
    erlang:send(Dest, {from_server, Data}).

send_to_client(Dest, Data) ->
    erlang:send(Dest, {to_client, Data}).

%% @doc 告诉进程socket已经准备好
send_ready_socket(Dest) ->
    erlang:send(Dest, ready_socket).
