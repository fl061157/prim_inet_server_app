%% @author fangliang
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_test/0]).




start_test() ->
	Inc = 0,
	loop( Inc ).

loop(10000) ->
	ok ;

loop(Inc) ->
	Parent = self() ,
	Pid = spawn_link( fun() -> connect(Parent) end  ) ,
	receive
		{ok, Pid} ->
			loop( Inc + 1) ;
		_ ->
			ok
	end .

connect(Parent) ->
	{ok , Socket} = gen_tcp:connect("192.168.1.241", 2222, [binary, {packet, 0}] ) ,
	Parent ! {ok, self() },
	ok = gen_tcp:send(Socket, "Some Data"),
   	receive_data(Socket,[]).

receive_data(Socket,SoFar) ->
     receive
        {tcp,Socket,Bin} ->
	       io:format("Receive Data ~s~n", [Bin]),
           receive_data(Socket,[Bin|SoFar]);
        {tcp_closed,Socket} ->
             list_to_binary(lists:reverse(SoFar))
     end.

%% ====================================================================
%% Internal functions
%% ====================================================================


