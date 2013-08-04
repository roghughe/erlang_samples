-module (config).
-compile (export_all).

-define(CONFIG_FILE,"priv/config.cfg").

%%--------------------------------------------------------------------
%% Function: Read the config file () -> {ok, Config} | 
%%                                      {error, Reason}
%% Description: Read the configuration data
%%--------------------------------------------------------------------
read() ->
  case read_1(get_root_path() ++ ?CONFIG_FILE) of
    {ok, C} -> {ok, C};
    {error, enoent} -> {error, no_file};
    Err -> Err
  end.

read_1(Location) ->
  case file:consult(Location) of
    {ok, C} -> C;
    O -> O
  end.

%%--------------------------------------------------------------------
%% Function: get (Key, Config) -> {error, not_found} |
%%                                {ok, Value}
%% Description: Get the value of a config element
%%--------------------------------------------------------------------
get(Key) -> get(Key, read()).

get(_Key, []) ->
  {error, not_found};
get(Key, [{Key, Value} | _Config]) ->
  {ok, Value};
get(Key, [{_Other, _Value} | Config]) ->
  get(Key, Config).

%%--------------------------------------------------------------------
%% Function: get_conf_path() -> "./priv/" | The appropriate 
%%                                          environment variable
%% Return the location of the app's private (priv) directory
%%--------------------------------------------------------------------
get_root_path() ->
	EnvParam = string:to_upper(erlang:atom_to_list(cache)),
	case os:getenv(EnvParam) of
  		false -> "./";
  		E -> E
	end.


