-module(meter_yaml).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [options/2, int/2, ip/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{listen =>
            options(
              #{port => int(0, 65535),
                ip => ip()},
              [unique, {return, map}, {defaults, default(listen)}])},
      [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
default(listen) ->
    #{port => 80,
      ip => {0, 0, 0, 0}}.
