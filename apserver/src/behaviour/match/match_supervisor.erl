-module(match_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
		  {error, {already_started, Pid :: pid()}} |
		  {error, {shutdown, term()}} |
		  {error, term()} |
		  ignore.
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
		  {ok, {SupFlags :: supervisor:sup_flags(),
				[ChildSpec :: supervisor:child_spec()]}} |
		  ignore.
init([]) ->

	SupFlags = #{strategy => one_for_one,
				 intensity => 1,
				 period => 5},

	TurnPlayer = #{id => make_ref(),
				   start => {turn_player, start_link, []},
				   restart => permanent,
				   shutdown => 5000,
				   type => worker,
				   modules => [turn_player]},

	NonTurnPlayer = #{id => make_ref(),
					  start => {non_turn_player, start_link, []},
					  restart => permanent,
					  shutdown => 5000,
					  type => worker,
					  modules => [non_turn_player]},

	MatchMoveDealer = #{id => make_ref(),
						start => {match_move_dealer, start_link, []},
						restart => permanent,
						shutdown => 5000,
						type => worker,
						modules => [match_move_dealer]},

	Children = [TurnPlayer, NonTurnPlayer, MatchMoveDealer],
	{ok, {SupFlags, Children} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
