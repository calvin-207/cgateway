%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 监控进程
%%%     1. gateway_server
%%%     2. gateway_listern_sup
%%% @end
%%% Created : 12. 八月 2016 13:42
%%%-------------------------------------------------------------------
-module(cgateway_sup).
-author("calvin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(cgateway_listen_server, worker), ?CHILD(cgateway_client_sup, supervisor)]}}.

