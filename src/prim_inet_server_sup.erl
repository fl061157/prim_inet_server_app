%% @author fangliang
%% @doc @todo Add description to prim_inet_server_sup.


-module(prim_inet_server_sup).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2,start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CLIENT_SUP, prim_inet_client_sup).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).



start_link(ListenPort, HandleModule) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [ListenPort,HandleModule]) .

start_child(Socket) ->
	supervisor:start_child(?CLIENT_SUP, [Socket]).

init([ListenPort, HandleModule]) ->
	PrimInetListener = { prim_inet_listener, 
						 {prim_inet_listener, start_link, [ListenPort, HandleModule]},
						 permanent, %% Restart Forever
						 2000,
						 worker,
						 [prim_inet_listener] } ,
	PrimInetSup = { ?CLIENT_SUP,
					{supervisor, start_link, [{local, ?CLIENT_SUP}, ?MODULE, [HandleModule]]} ,
					permanent,
                    infinity,
                    supervisor,
                    [] } ,
	Child = [PrimInetListener , PrimInetSup ] ,
	RestartStrategy = { one_for_one, ?MAX_RESTART, ?MAX_TIME } ,
	io:format("PRIM_INET_SERVER START INIT 1 ~n"),
	{ok, {RestartStrategy , Child }};

init([HandleModule]) ->
	Server = { undefined, 
			   {HandleModule, start_link, []},
			   temporary,
			   2000,
			   worker,
			   [] } ,
	Child = [Server] ,
	RestartStrategy = {simple_one_for_one, ?MAX_RESTART , ?MAX_TIME } ,
	{ok, { RestartStrategy , Child}} .



%% ====================================================================
%% Internal functions
%% ====================================================================


