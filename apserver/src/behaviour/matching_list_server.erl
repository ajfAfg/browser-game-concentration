-module(matching_list_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_matching/2]).
-export([delete_matching/1, delete_matching/2]).
-export([search_matching/1, search_matching/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, format_status/2]).

-include_lib("src/behaviour/config.hrl").
-include_lib("src/behaviour/match.hrl").

-define(SERVER, ?MODULE).

-type state() :: list(matching_info()).

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

add_matching(MatchingId, UserIds) ->
	gen_server:cast(?SERVER, {add,MatchingId,UserIds}).

delete_matching(MatchingId) ->
	gen_server:cast(?SERVER, {delete,MatchingId}).

delete_matching(MatchingId, UserId) ->
	gen_server:cast(?SERVER, {delete,MatchingId,UserId}).

search_matching(MatchingId) ->
	gen_server:call(?SERVER, {search,MatchingId}).

search_matching(MatchingId, UserId) ->
	gen_server:call(?SERVER, {search,MatchingId,UserId}).

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
handle_call({search,MatchingId,UserId}, _From, State) ->
	Reply = case lists:keyfind(MatchingId, 1, State) of
				{_, UserIds} ->
					lists:member(UserId,UserIds);
				false ->
					false
			end,
	{reply, Reply, State};
handle_call({search,MatchingId}, _From, State) ->
	Reply = lists:keymember(MatchingId, 1, State),
	{reply, Reply, State}.

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
handle_cast({add,MatchingId,UserIds}, State) ->
	NewState = [{MatchingId,UserIds} | State],
	{noreply, NewState};
handle_cast({delete,MatchingId}, State) ->
	NewState = lists:keydelete(MatchingId, 1, State),
	{noreply, NewState};
handle_cast({delete,MatchingId,UserId}, State) ->
	{_, UserIds} = lists:keyfind(MatchingId, 1, State),
	NewMatching = {MatchingId, lists:delete(UserId,UserIds)},
	NewState = lists:keyreplace(MatchingId, 1, State, NewMatching),
	{noreply, NewState}.

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
