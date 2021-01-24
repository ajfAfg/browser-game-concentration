-module(concentration_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
-spec start_link(Args :: list()) -> {ok, Pid :: pid()} |
		  {error, {already_started, Pid :: pid()}} |
		  {error, {shutdown, term()}} |
		  {error, term()} |
		  ignore.
start_link(Args) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

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

	Child1 = #{id => make_ref(),
			   start => {room_server, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => worker,
			   modules => [room_server]},

	Child2 = #{id => make_ref(),
			   start => {matching_list_server, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => worker,
			   modules => [matching_list_server]},

	Child3 = #{id => make_ref(),
			   start => {providing_deck_server, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => worker,
			   modules => [providing_deck_server]},

	Child4 = #{id => make_ref(),
			   start => {deciding_first_player_server, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => worker,
			   modules => [deciding_first_player_server]},

	MatchSup = #{id => make_ref(),
			   start => {match_supervisor, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => supervisor,
			   modules => [match_supervisor]},

	Children = [Child1, Child2, Child3, Child4, MatchSup],
	{ok, {SupFlags, Children} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
