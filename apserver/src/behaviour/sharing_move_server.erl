-module(sharing_move_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([fetch_opponent_move/1]).
-export([share_this_move/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, format_status/2]).

-include_lib("src/behaviour/config.hrl").
-include_lib("src/behaviour/match.hrl").

-define(SERVER, ?MODULE).

-type x() :: string().
-type y() :: string().
-type move() :: {x(), y()}.
-type state() :: list({matching_id(), pid()}).

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

-spec fetch_opponent_move(MatchingId :: matching_id()) -> move().
fetch_opponent_move(MatchingId) ->
	gen_server:call(?SERVER, {fetch,MatchingId}),
	receive
		{?SERVER, {move, Move}} ->
			Move
	after ?ALLOTTED_TIME ->
			timeout
	end.

-spec share_this_move(MatchingId :: matching_id(), Move :: move()) -> ok.
share_this_move(MatchingId, Move) ->
	case gen_server:call(?SERVER, {share,MatchingId,Move}) of
		ok ->
			ok;
		no ->
			share_this_move(MatchingId, Move)
	end.

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
	{ok, []}.

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
handle_call({fetch,MatchingId}, {From,_Tag}, State) when not (is_list(MatchingId) andalso is_pid(From)) ->
	{reply, badarg, State};
handle_call({fetch,MatchingId}, {From,_Tag}, State) ->
	NewState = [{MatchingId,From} | State],
	{reply, ok, NewState};

handle_call({share,MatchingId,Move}, _From, State) when not (is_list(MatchingId) andalso is_tuple(Move)) ->
	{reply, badarg, State};
handle_call({share,MatchingId,Move}, _From, State) ->
	Opponents = lists:filter(fun({Key,_}) -> Key=:=MatchingId end, State),
	case length(Opponents) =:= ?PLAYER_NUM-1 of
		true ->
			spawn(fun() -> share(Opponents,Move) end),
			NewState = lists:subtract(State, Opponents),
			{reply, ok, NewState};
		false ->
			{reply, no, State}
	end.


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
-spec share(Opponents :: state(), Move :: move()) -> ok.
share(Opponents, Move) ->
	{_, Pids} = lists:unzip(Opponents),
	Fun = fun(Pid) ->
				  Pid ! {?SERVER, {move,Move}}
		  end,
	lists:foreach(Fun, Pids).
