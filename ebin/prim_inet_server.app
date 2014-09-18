{ application,prim_inet_server,
  [
  	{description,"Prim Inet Tcp Server Echo"},
  	{vsn,"1.0.0"},
  	{modules,[prim_inet_server_app,prim_inet_server_sup,prim_inet_listener,prim_inet_client_handler]},
  	{registered,[prim_inet_server_sup]},
  	{applications,[kernel,stdlib]},
  	{mod,{prim_inet_server_app,[]}}
  ]
}.