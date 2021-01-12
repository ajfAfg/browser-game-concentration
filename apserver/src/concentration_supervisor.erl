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

	Child1 = #{id => tag1,
			   start => {room_server, start_link, []},
			   restart => permanent,
			   shutdown => 5000,
			   type => worker,
			   modules => [room_server]},

	{ok, {SupFlags, [Child1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
