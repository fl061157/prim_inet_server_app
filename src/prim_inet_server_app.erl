%% @author fangliang
%% @doc @todo Add description to prim_inet_server_app.


-module(prim_inet_server_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1]).

-define(DEFAULT_PORT, 2222).
-define(HANDLE_MODULE, prim_inet_client_handler).


start(_Type, _Args) ->
	ListenPort = get_app_env(listen_port, ?DEFAULT_PORT),
	case prim_inet_server_sup:start_link(ListenPort, ?HANDLE_MODULE ) of
		{ok, Pid} -> {ok, Pid} ;
		Other -> {error, Other}
	end.

stop(_State) ->
        ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_app_env(Operation, DEF_PORT) ->
	case application:get_env(application:get_application(), Operation) of
		{ok, Val} -> Val;
		_ -> 
			case init:get_argument(Operation) of
				[[Val | _]] -> Val;
                error -> DEF_PORT
			end
	end.


