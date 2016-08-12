%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2016 13:49
%%%-------------------------------------------------------------------
-module(cgateway_client_sup).
-author("calvin").

-behaviour(supervisor).

%% API
-export([start_link/0,
    start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ClientSocket) ->
    supervisor:start_child(?MODULE, [ClientSocket]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(cgateway_listen_server, worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
