-module(turn_player).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([tell_other_players_my_move/4]).
-export([get_turn_move/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, format_status/2]).

-include_lib("src/behaviour/config.hrl").
-include_lib("src/behaviour/match_types.hrl").

-define(SERVER, ?MODULE).

-type move_info() :: #{user_id => user_id(), move => move()}.
-type turn_infos() :: #{turn() => move_info()}.
-type state() :: #{matching_id() => turn_infos()}.

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

-spec tell_other_players_my_move(
		MatchingId :: matching_id(),
		UserId :: user_id(),
		Turn :: turn(),
		Move :: move()
	   ) -> ok | timeout.
tell_other_players_my_move(MatchingId, UserId, Turn, Move)
  when not (is_list(MatchingId) andalso
			is_list(UserId) andalso
			is_integer(Turn) andalso
			is_tuple(Move)
		   ) ->
	badarg;
tell_other_players_my_move(MatchingId, UserId, Turn, Move) ->
	gen_server:call(?SERVER, {tell,MatchingId,UserId,Turn,Move}),
	match_move_dealer:share_move(MatchingId, UserId, Turn),
	receive
		{match_move_dealer, {share,_Move} } ->
			ok
	after ?ALLOTTED_TIME ->
		timeout
	end.

-spec get_turn_move(MatchingId :: matching_id(), Turn :: turn()) ->
		  {UserId :: user_id(), Move :: move()} | no_move.
get_turn_move(MatchingId, Turn) when not (is_list(MatchingId) andalso is_integer(Turn)) ->
	badarg;
get_turn_move(MatchingId, Turn) ->
	gen_server:call(?SERVER, {get,MatchingId,Turn}).

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
handle_call({tell,MatchingId,UserId,Turn,Move}, _From, State) ->
	TurnInfos = case maps:find(MatchingId,State) of
				   {ok, Maps} ->
					   Maps;
				   error ->
					   #{}
				end,
	MoveInfo = #{
				 user_id => UserId,
				 move => Move
				},
	NewInfos = TurnInfos#{Turn => MoveInfo},
%	NewState = maps:update(MatchingId, NewInfos, State),
	NewState = State#{MatchingId => NewInfos},
	{reply, ok, NewState};

handle_call({get,MatchingId,Turn}, _From, State) ->
	TurnInfos = case maps:find(MatchingId,State) of
				   {ok, Maps1} ->
					   Maps1;
				   error ->
					   #{}
				end,
	MoveInfo = case maps:find(Turn,TurnInfos) of
			   {ok, Maps2} ->
%					list_to_tuple(maps:values(Maps2) );
					   Maps2;
				error ->
					no_move
			   end,
	{reply, MoveInfo, State}.

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
