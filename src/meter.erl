-module(meter).

-behaviour(application).

%% API
-export([start/0, stop/0]).
-export([init/1, init/2]).
-export([set/2, set/3]).
-export([inc/1, inc/2, inc/3]).
-export([observe/2, observe/3]).
-export([list/0]).
-export([cowboy_routes/0]).
-export([cowboy_env/0]).
%% Application callbacks
-export([start/2, stop/1, prep_stop/1, config_change/3]).
-export_type([name/0, type/0, label/0, opts/0]).

-type bucket() :: number() | infinity.

-type opts() :: #{name := name(),
                  help := string(),
                  labels => [label()],
                  buckets => [bucket()]}.
-type name() :: atom().
-type type() :: histogram | counter | gauge | summary | boolean.
-type label() :: term().

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    case application:stop(?MODULE) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        {error, _} = Err -> Err
    end.

-spec init([{type(), opts()}]) -> ok.
init(List) ->
    lists:foreach(
      fun({Type, Opts}) ->
              init(Type, Opts)
      end, List).

-spec init(type(), opts()) -> ok.
init(Type, #{name := Name, help := _} = Opts) ->
    Mod = mod_name(Type),
    try Mod:new(maps:to_list(Opts)) of
        ok ->
            persistent_term:put({?MODULE, Name}, {Mod, Type})
    catch _:{mf_already_exists, _, _} ->
            ok
    end;
init(Type, Opts) ->
    erlang:error(badarg, [Type, Opts]).

-spec set(name(), term()) -> ok.
set(Name, Val) ->
    set(Name, [], Val).

-spec set(name(), [label()], term()) -> ok.
set(Name, Labels, Val) ->
    Mod = get_module(Name),
    Mod:set(Name, Labels, Val).

-spec inc(name()) -> ok.
inc(Name) ->
    inc(Name, 1).

-spec inc(name(), term()) -> ok.
inc(Name, Labels) when is_list(Labels) ->
    inc(Name, Labels, 1);
inc(Name, Val) ->
    inc(Name, [], Val).

-spec inc(name(), [label()], term()) -> ok.
inc(Name, Labels, Val) ->
    Mod = get_module(Name),
    Mod:inc(Name, Labels, Val).

-spec observe(name(), term()) -> ok.
observe(Name, Val) ->
    observe(Name, [], Val).

-spec observe(name(), [label()], term()) -> ok.
observe(Name, Labels, Val) ->
    Mod = get_module(Name),
    Mod:observe(Name, Labels, Val).

-spec list() -> [{name(), type()}].
list() ->
    lists:filtermap(
      fun({{?MODULE, Name}, {_, Type}}) ->
              {true, {Name, Type}};
         (_) ->
              false
      end, persistent_term:get()).

-spec cowboy_routes() -> term().
cowboy_routes() ->
    meter_httpd:routes().

-spec cowboy_env() -> map().
cowboy_env() ->
    meter_httpd:env().

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case meter_sup:start_link() of
        {ok, _} = Ok ->
            case meter_httpd:start() of
                ok -> Ok;
                Err -> Err
            end;
        Err ->
            Err
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    lists:foreach(
      fun({Name, _}) ->
              persistent_term:erase({?MODULE, Name})
      end, list()).

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    _ = meter_httpd:stop(),
    State.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(Changed, New, Removed) ->
    _ = meter_httpd:config_change(Changed, New, Removed),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec mod_name(type()) -> module().
mod_name(Type) ->
    list_to_atom("prometheus_" ++ atom_to_list(Type)).

-spec get_module(name()) -> module().
get_module(Name) ->
    try persistent_term:get({?MODULE, Name}) of
        {Mod, _} -> Mod
    catch _:badarg ->
            erlang:error({metric_not_found, Name})
    end.
