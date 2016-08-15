%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2016 13:49
%%%-------------------------------------------------------------------
-module(cgateway_client_server).
-author("calvin").

-behaviour(gen_server).
-include("cgateway.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    socket,    % client socket
    addr,       % client address
    id = 0       %%
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ClientSocket :: port()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CliSocket) ->
    gen_server:start_link(?MODULE, [CliSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Socket]) ->
    erlang:process_flag(trap_exit, true),
    {ok, {IP, Port}} = inet:peername(Socket),
    {ok, #state{socket = Socket, addr = {IP, Port}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Bin}, #state{socket = Socket, id = ID} = State) when ID > 0 ->
    Decode = cgateway_parser:decode(Bin),
    cgateway_router:router(ID, Decode),
    %% 处理完再接受信息
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket, addr = Addr, id = ID} = StateData) ->
    error_logger:info_msg("~p Client ~p id = ~p disconnected.\n", [self(), Addr, ID]),
    {stop, normal, StateData};
handle_info(ready_socket, #state{socket = Socket} = State) ->
    prim_inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {noreply, State};
handle_info({start_role, ID}, #state{addr = {IP, Port}} = State) ->
    do_start_role(ID, IP, Port),
    {noreply, State#state{id = ID}};
handle_info({from_server, Data}, #state{addr = Addr} = State) ->
    error_logger:info_msg("~p Client ~p received an unknow from_server msg = ~p disconnected.\n", [self(), Addr, Data]),
    {noreply, State};
handle_info({to_client, Data}, #state{socket = Socket}=State) ->
    EncodeData = cgateway_parser:encode(Data),
    gen_tcp:send(Socket, EncodeData),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc login, 启动role_server， 并把相关信息交付过去。
do_init_msg(ID, IP, Port) ->
    case erlang:whereis(role_util:get_procress_id(ID)) of
        undefined -> %% start role server
            role_sup:start_child(ID, IP, Port);
        PID -> %% role process exist update it
            role_sup:update_child(PID, ID, IP, Port)
    end.

%%%===================================================================
%%% procress dictionary
%%%===================================================================


