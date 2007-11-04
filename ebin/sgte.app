{application, sgte, 
	[{description, "sgte - a simple Erlang template engine"},
	{vsn, "0.7.0"},
	{modules, [sgte, 
                   sgte_parse, 
                   sgte_render, 
                   sgte_gettext,
                   sgte_dict]},
	{registered, []},
	{applications, [kernel, stdlib]}
	]}.
