%% @author fangliang
%% @doc @todo Add description to prim_inet_client_handler.


-module(prim_inet_client_handler).

-behavior(gen_server).

-record(state,{socket,ip, port}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, init/1]).

-export([handle_info/2, handle_call/3, handle_cast/2, terminate/2 , code_change/3]).


start_link(Socket) ->
	io:format("Start Client Hanlder ...... ~n"),
	gen_server:start_link(?MODULE, [Socket], [] ).

init([Socket]) ->
	inet:setopts(Socket, [binary,{active,once}]) ,
	{ok, { IP, Port } } = inet:peername(Socket) ,
	{ok, #state{ socket = Socket, ip = IP, port = Port } } .


handle_info({ tcp, Socket, Data }, State ) ->
	inet:setopts(Socket, [{active, once}]) ,
    io:format("Receive Data : ~p ~n",[Data]),
	gen_tcp:send(Socket,  io_lib:fwrite("Echo back : ~p~n", [Data]) ),
	{noreply, State} ;

handle_info( {tcp_closed, _Socket} , #state{ip = IP, port = Port } = StateData ) ->
	error_logger:info_msg("~p Client ~p:~p disconnected.\n", [self(), IP, Port]),
	{stop, normal, StateData } ;

handle_info(_Info, State) ->
	{noreply , State} .

handle_call(_Request, _From, State) ->
	{noreply, ok, State} .

handle_cast(_Msg, State) ->
	{noreply, State} .

terminate(_Reason, #state{socket=Socket} ) ->
	( catch gen_tcp:close(Socket) ) ,
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State} .


%% ====================================================================
%% Internal functions
%% ====================================================================


