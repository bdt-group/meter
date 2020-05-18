-module(meter_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================
load_config_test() ->
    ?assertEqual(ok, conf:start()),
    ?assertEqual(ok, conf:load_file(filename:join(test_dir(), "1.yml"))).

start_test() ->
    ?assertEqual(ok, meter:start()).

double_start_test() ->
    start_test().

init_test() ->
    ?assertEqual(
       ok,
       meter:init(
         lists:map(
           fun(Type) ->
                   {Type, #{name => name(Type), help => "Help"}}
           end, types()))).

double_init_test() ->
    init_test().

inc_test() ->
    ?assertEqual(ok, meter:inc(name(counter))),
    ?assertEqual(ok, meter:inc(name(counter), [])),
    ?assertEqual(ok, meter:inc(name(counter), 5)),
    ?assertEqual(ok, meter:inc(name(gauge))),
    ?assertEqual(ok, meter:inc(name(gauge), [])),
    ?assertEqual(ok, meter:inc(name(gauge), 5)).

set_test() ->
    ?assertEqual(ok, meter:set(name(gauge), 1)),
    ?assertEqual(ok, meter:set(name(boolean), true)).

observe_test() ->
    ?assertEqual(ok, meter:observe(name(summary), 1)),
    ?assertEqual(ok, meter:observe(name(histogram), 1)).

missing_opts_test() ->
    Type = counter,
    ?assertError(badarg, meter:init(Type, #{})),
    ?assertError(badarg, meter:init(Type, #{name => foo})),
    ?assertError(badarg, meter:init(Type, #{help => "Help"})).

non_existent_test() ->
    ?assertError({metric_not_found, foo}, meter:inc(foo, 1)).

reload_config_test() ->
    ?assertEqual(ok, conf:reload_file(filename:join(test_dir(), "2.yml"))).

http_api_test() ->
    ?assertMatch([_|_], meter:cowboy_routes()),
    ?assertMatch(#{}, meter:cowboy_env()).

http_request_test() ->
    ?assertEqual(ok, conf:reload_file(filename:join(test_dir(), "3.yml"))),
    Ret = httpc:request("http://localhost:50505/metrics"),
    ?assertMatch({ok, {{_, 200, _}, _, _}}, Ret),
    %% Check that all metrics are exposed
    {_, {{_, _, _}, _, Data}} = Ret,
    Lines = string:split(Data, "\n", all),
    ?assertEqual(
       [],
       lists:filter(
         fun(Type) ->
                 Name = atom_to_list(name(Type)),
                 not lists:any(
                       fun(Line) ->
                               lists:prefix(Name, Line)
                       end, Lines)
         end, types())).

list_test() ->
    Expect = lists:sort([{name(Type), Type} || Type <- types()]),
    ?assertEqual(Expect, lists:sort(meter:list())).

stop_test() ->
    ?assertEqual(ok, meter:stop()),
    ?assertEqual([], meter:list()).

double_stop_test() ->
    stop_test().

%%%===================================================================
%%% Internal functions
%%%===================================================================
types() ->
    [histogram, counter, gauge, summary, boolean].

name(Type) ->
    list_to_atom("eunit_test_" ++ atom_to_list(Type)).

test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, "test").
