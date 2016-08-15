%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% gateway handle some message
%%% @end
%%% Created : 15. 八月 2016 10:35
%%%-------------------------------------------------------------------
-module(cgateway_handler).
-author("calvin").

%% API
-export([role_login/1]).

role_login(_Data) ->
    case catch check_login() of
        ok ->
            ok;

    end.

check_login(Platform) ->
    %% 检查服务器状态
    case server_is_open of
        true ->
            ok;
        false ->
            check_white_ip_imei
    end,
    %% 检查sdk
    case check_sdk of
        true ->
            ok;
        false ->
            error_sdk
    end,
    %% 检车是否排队
    case check_queue of
        false -> %% 无需排队
            login;
        true ->
            check_white_ip_imei %% 判断是否是白名单、ip、mac地址
    end.