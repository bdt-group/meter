# meter

A handy wrapper around metrics collector.
Currently supported collector is [Prometheus](https://github.com/deadtrickster/prometheus.erl).

## Usage

## Initialization

### Configuring HTTP exporter

There are two modes available:

- standalone: `meter` starts its own HTTP exporter (currently via [cowboy](https://github.com/ninenines/cowboy)).
  This is controlled by `listen` option.

- embedded: the exporter is started within existing `cowboy` HTTP listener.

In order to configure in standalone mode, `listen` option must be set, e.g.:
```erl
[
  {meter,
    [{listen, #{port => 8080,
                ip => {0, 0, 0, 0}}}]},
  ...
].
```
For embedded mode, `meter:cowboy_routes/0` and `meter:cowboy_env/0` functions should be used, e.g.:
```erl
start_cowboy(Port) ->
   Routes = [{'_', [{"/route1", my_handler1, []},
                    {"/route2", my_handler2, []}
                    | meter:cowboy_routes()]}],
   Dispatch = cowboy_router:compile(Routes),
   Env = maps:merge(meter:cowboy_env(), #{env => #{dispatch => Dispatch}}),
   cowboy:start_clear(?MODULE, [{port, Port}], Env).
```

### Declaring metrics

All existing metrics must be initialized using functions `meter:init/1` or `meter:init/2`, e.g.:
```erl
init_metrics() ->
   meter:init(
      [{counter, #{name => packets_total,
                   help => "Number of packets",
                   labels => [type]}},
       {counter, #{name => unexpected_packets_total,
                   help => "Number of unexpected packets",
                   labels => [type]}}]).
```

## Usage

Currently supported metric types are:

- `histogram` - partial support
- `counter` - full support
- `gauge` - full support (decrement is done via negative increment)
- `summary` - partial support
- `boolean` - full support

You don't need to provide the type during operations with metrics, because a metric name
is coupled with its type during initialization and there cannot be two metrics with the
same name but different types.

### API

The following functions are available for operating on metrics:

- [meter:inc/1](#inc), [meter:inc/2](#inc), [meter:inc/3](#inc)
- [meter:set/2](#set), [meter:set/3](#set)
- [meter:observe/2](#observe), [meter:observe/3](#observe)

#### inc

```erl
-spec inc(Name :: name()) -> ok.
-spec inc(Name :: name(), Inc :: integer() | number()) -> ok.
-spec inc(Name :: name(), [Label :: label()], Inc :: integer() | number()) -> ok.

-type name() :: atom().
-type label() :: atom().
```
Increments metric `Name`, where `Inc` must be a positive integer for `counter`
and an integer or a number for `gauge`.
This function can be called for `counter` and `gauge` metrics only.

#### set

```erl
-spec set(Name :: name(), Val :: term()) -> ok.
-spec set(Name :: name(), [Label :: label()], Val :: term()) -> ok.

-type name() :: atom().
-type label() :: atom().
```
Sets value `Val` for a metric `Name`.
This function can be called for `gauge` and `boolean` metrics only.

#### observe

```erl
-spec observe(name(), term()) -> ok.
-spec observe(name(), [label()], term()) -> ok.

-type name() :: atom().
-type label() :: atom().
```
Updates value `Val` for a metric `Name`.
This function can be called for `summary` and `histogram` metrics only.
