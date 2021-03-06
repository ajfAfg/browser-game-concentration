-module(room_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([wait_for_matching/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, format_status/2]).

-include_lib("src/behaviour/config.hrl").
-include_lib("src/behaviour/match_types.hrl").

-define(SERVER, ?MODULE).

-type state() :: #{user_id() => pid()}.

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

wait_for_matching(UserId) ->
	gen_server:call(?SERVER, {reservation,UserId}),
	receive
		{?SERVER, {matching, MatchingId}} ->
			{matching, MatchingId}
	after ?MATCHING_WAIT_TIME ->
			gen_server:cast(?SERVER, {delete,UserId}),
			timeout
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
handle_call({reservation,UserId}, {From,_Tag}, State) ->
	NewState = case State#{UserId => From} of
				   S ->
					   case maps:size(S) =:= ?PLAYER_NUM of
						   true ->
							   spawn(fun() -> match(S) end),
							   #{};
						   false ->
							   S
					   end
			   end,
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
handle_cast({delete,UserId}, State) ->
	NewState = maps:remove(UserId, State),
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
					Status :: list()) -> Status :: state().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec match(State :: state()) -> ok.
match(State) ->
	{UserIds, Pids} = lists:unzip(maps:to_list(State) ),
	MatchingId = generate_matching_id(),

	providing_deck_server:generate_deck(MatchingId),
	io:format("~p: ~p~n", [MatchingId, providing_deck_server:request_deck(MatchingId)]),
	deciding_first_player_server:decide_first_player(MatchingId, UserIds),
	matching_list_server:add_matching(MatchingId, UserIds),

	Fun = fun(Pid) ->
				  Pid ! {?SERVER, {matching,MatchingId} }
		  end,
	lists:foreach(Fun, Pids).

-spec generate_matching_id() -> string().
generate_matching_id() ->
	make_list(fun random_char/0, ?MATCHING_ID_STRENGTH).
	%binary_to_list(base64:encode(crypto:strong_rand_bytes(?MATCHING_ID_STRENGTH)) ).

make_list(F, N) ->
	[F() || _ <- lists:seq(0,N)].

random_char() ->
	rand:uniform($Z-$A+1) - 1 + $A.
