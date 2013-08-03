-module (config).
-compile (export_all).

-define(CONFIG_FILE,"config/config.cfg").

%%--------------------------------------------------------------------
%% Function: Read the config file () -> {ok, Config} | 
%%                                      {error, Reason}
%% Description: Read the configuration data
%%--------------------------------------------------------------------
read() ->
  case read_1(?CONFIG_FILE) of
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