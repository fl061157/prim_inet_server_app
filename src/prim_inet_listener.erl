%% @author fangliang
%% @doc @todo Add description to prim_inet_listener.


-module(prim_inet_listener).

-behaviour(gen_server).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).
-export([init/1,handle_info/2,handle_call/3,handle_cast/2,terminate/2,code_change/3]).

-define(SERVER , ?MODULE). 
-record(state , {listener, acceptor, module}).

start_link(ListenPort, HandleModule) 
	when is_integer(ListenPort) , is_atom(HandleModule) ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenPort , HandleModule], []).

init([ListenPort, HandleModule]) ->
  	process_flag(trap_exit, true) ,
	Opertion = [binary, {active, false} , {reuseaddr, true} , {keepalive, true} ] ,
	case gen_tcp:listen(ListenPort, Opertion) of
		{ ok, LSock } -> 
			{ ok, Ref } = prim_inet:async_accept(LSock, -1),
			{ok, #state{listener = LSock , acceptor = Ref , module = HandleModule}} ;
		{error, Reason} ->
			{stop, Reason}
	end.
			
handle_info({inet_async , LSock, Ref, {ok, ClientSocket}} , #state{listener=LSock, acceptor=Ref, module=_Module} = State) ->
	try
		case set_sockopt(LSock, ClientSocket) of
			ok -> ok;
			{error, Reason} -> exit({set_sockopt, Reason}) 
		end,
		{ok, Pid} = prim_inet_server_sup:start_child( ClientSocket ) ,
		gen_tcp:controlling_process( ClientSocket , Pid ) ,
		case prim_inet:async_accept(LSock, -1) of
			{ok, NewRef} -> ok;
			{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)}) 
		end,
		{noreply , State#state{acceptor = NewRef}}
	catch exit:Why ->
		error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
		{stop, Why, State}
	end;

handle_info({inet_async, LSock, Ref, Error}, #state{listener=LSock, acceptor=Ref } = State) ->
	 error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
	 {stop, Error, State};

handle_info(_Info, State) ->
	{noreply, State}.
	
handle_call(Request, _From, State) ->
	{stop, {unknown_call, Request} , State} .

handle_cast(_Msg,State) ->
	{noreply , State}.

terminate(_Reason,State) ->
	gen_tcp:close( State#state.listener), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




%% ====================================================================
%% Internal functions
%% ====================================================================

set_sockopt(LSock, ClientSocket) ->
	true = inet_db:register_socket(ClientSocket , inet_tcp) ,
	case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Operations} ->
			case prim_inet:setopts(ClientSocket, Operations) of
				ok -> ok;
				Error -> gen_tcp:close(ClientSocket) , Error
			end;
		Error ->
			gen_tcp:close(ClientSocket) , Error
	end.


