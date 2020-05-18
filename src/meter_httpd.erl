-module(meter_httpd).

%% API
-export([start/0]).
-export([start/1]).
-export([stop/0]).
-export([config_change/3]).
-export([routes/0]).
-export([env/0]).

-include_lib("kernel/include/logger.hrl").

-type options() :: #{ip := inet:ip_address(),
                     port := inet:port_number()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:get_env(meter, listen) of
        {ok, Opts} ->
            case start(Opts) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end;
        undefined ->
            ok
    end.

-spec start(options()) -> {ok, pid()} | {error, term()}.
start(#{ip := IP, port := Port}) ->
    ?LOG_NOTICE("Starting HTTP metrics listener at ~s:~B", [format_ip(IP), Port]),
    Dispatch = cowboy_router:compile([{'_', routes()}]),
    Env = maps:merge(env(), #{env => #{dispatch => Dispatch}}),
    cowboy:start_clear(?MODULE, [{port, Port}, {ip, IP}], Env).

-spec stop() -> ok | {error, term()}.
stop() ->
    ?LOG_NOTICE("Stopping HTTP metrics listener"),
    cowboy:stop_listener(?MODULE).

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok | {error, term()}.
config_change(Changed, New, Removed) ->
    case {proplists:get_value(listen, Changed),
          proplists:get_value(listen, New),
          lists:member(listen, Removed)} of
        {undefined, undefined, false} -> ok;
        {undefined, undefined, true} -> stop();
        {undefined, Opts, _} -> start(Opts);
        {Opts, _, _} ->
            _ = stop(),
            start(Opts)
    end.

-spec routes() -> [{string(), module(), term()}].
routes() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}].

-spec env() -> map().
env() ->
    #{metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
      stream_handlers => [cowboy_metrics_h, cowboy_stream_h]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec format_ip(inet:ip_address()) -> string().
format_ip({_, _, _, _} = IP4) ->
    inet_parse:ntoa(IP4);
format_ip(IP6) ->
    "[" ++ inet_parse:ntoa(IP6) ++ "]".
