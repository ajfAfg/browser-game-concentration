-module(match_move_dealer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([share_move/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, format_status/2]).

-include_lib("src/behaviour/config.hrl").
-include_lib("src/behaviour/match_types.hrl").

-define(SERVER, ?MODULE).

-type player() :: {user_id(), pid()}.
-type turn_info() :: {turn(), list(player())}.
-type state() :: #{matching_id() => turn_info()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		  {error, Error :: {already_started, pid()}} |
		  {error, Error :: term()} |
		  ignore.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec share_move(
		MatchingId :: matching_id(),
		UserId :: user_id(),
		Turn :: turn()
	   ) -> ok.
share_move(MatchingId, UserId, Turn) ->
	gen_server:call(?SERVER, {share,MatchingId,UserId,Turn}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: state()} |
		  {ok, State :: state(), Timeout :: timeout()} |
		  {ok, State :: state(), hibernate} |
		  {stop, Reason :: term()} |
		  ignore.
init([]) ->
	process_flag(trap_exit, true),
	{ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
		  {reply, Reply :: term(), NewState :: state()} |
		  {reply, Reply :: term(), NewState :: state(), Timeout :: timeout()} |
		  {reply, Reply :: term(), NewState :: state(), hibernate} |
		  {noreply, NewState :: state()} |
		  {noreply, NewState :: state(), Timeout :: timeout()} |
		  {noreply, NewState :: state(), hibernate} |
		  {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
		  {stop, Reason :: term(), NewState :: state()}.
handle_call({share,MatchingId,UserId,Turn}, {From,_Tag}, State) ->
	{Turn,Players} = case maps:find(MatchingId,State) of
						 {ok, Tuple} ->
							 Tuple;
						 error ->
							 {Turn, []}
					 end,
	NewPlayers = case lists:keymember(UserId,1,Players) of
					 true ->
						 Players;
					 false ->
						 [{UserId,From} | Players]
				 end,
	NewTurnInfo = case length(NewPlayers) =:= ?PLAYER_NUM of
					  true ->
						  {_, Froms} = lists:unzip(NewPlayers),
						  spawn(fun() -> share(MatchingId,Turn,Froms) end),
						  {};
					  false ->
						  {Turn, NewPlayers}
				  end,
	NewState = State#{MatchingId => NewTurnInfo},
	{reply, ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: state()) ->
		  {noreply, NewState :: state()} |
		  {noreply, NewState :: state(), Timeout :: timeout()} |
		  {noreply, NewState :: state(), hibernate} |
		  {stop, Reason :: term(), NewState :: state()}.
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: state()) ->
		  {noreply, NewState :: state()} |
		  {noreply, NewState :: state(), Timeout :: timeout()} |
		  {noreply, NewState :: state(), hibernate} |
		  {stop, Reason :: normal | term(), NewState :: state()}.
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
				State :: state()) -> any().
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
				  State :: state(),
				  Extra :: term()) -> {ok, NewState :: state()} |
		  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
					Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec share(MatchingId :: matching_id(), Turn :: turn(), Pids :: list(pid()) ) -> ok.
share(MatchingId, Turn, Pids) ->
	#{move := Move} = turn_player:get_turn_move(MatchingId,Turn),
	Fun = fun(Pid) ->
				  Pid ! {?SERVER, {share,Move} }
		  end,
	lists:foreach(Fun, Pids).
