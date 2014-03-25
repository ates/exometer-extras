-module(exometer_report_kairosdb).

-behaviour(exometer_report).

-export([exometer_init/1]).
-export([exometer_call/3]).
-export([exometer_cast/2]).
-export([exometer_info/2]).
-export([exometer_report/5]).
-export([exometer_subscribe/5]).
-export([exometer_unsubscribe/4]).
-export([exometer_newentry/2]).
-export([exometer_setopts/4]).
-export([exometer_terminate/2]).

-include_lib("exometer/include/exometer.hrl").

-define(DEFAULT_URL, "http://localhost:8080/api/v1/datapoints").
-define(DEFAULT_INTERVAL, 10000). % 10 seconds

-define(INTERVAL(Opts, State), proplists:get_value(interval, Opts, State#state.interval)).

-record(state, {
    url = ?DEFAULT_URL :: string(),
    interval = ?DEFAULT_INTERVAL :: pos_integer(),
    tags = [] :: [proplists:property()]
}).

exometer_init(Options) ->
    URL = proplists:get_value(url, Options, ?DEFAULT_URL),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    Tags = proplists:get_value(tags, Options, []),
    {ok, #state{url = URL, interval = Interval, tags = Tags}}.

exometer_report(Metric, DataPoint, _Extra, Value, State) ->
    JSON = case Metric of
        [App, Name] ->
            [
                {name, make_name(App, Name, DataPoint)},
                {timestamp, estd_datetime:timestamp(msec)},
                {value, Value},
                {tags, State#state.tags}
            ];
        [App, Name | Rest] when length(Rest) rem 2 == 0 ->
            [
                {name, make_name(App, Name, DataPoint)},
                {timestamp, estd_datetime:timestamp(msec)},
                {value, Value},
                {tags, make_tags(Rest) ++ State#state.tags}
            ]
    end,
    Body = jsx:encode([JSON]),
    lager:debug("Push JSON to KairosDB: ~p", [Body]),
    try
        case lhttpc:request(State#state.url, "POST", [], Body, 5000) of
            {ok, {{204, _}, _, _}} -> ok;
            Error ->
                lager:error("Can't push metric to KairosDB: ~p", [Error])
        end
    catch
        _:Reason ->
            lager:error("Can't push metric to KairosDB: ~p", [Reason])
    end,
    {ok, State}.

exometer_call(_Request, _From, State) ->
    {ok, State}.

exometer_cast(_Request, State) ->
    {ok, State}.

exometer_info(_Msg, State) ->
    {ok, State}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_newentry(#exometer_entry{name = Name, type = Type, options = Options}, State) ->
    case lists:keyfind(kairosdb, 1, Options) of
        {kairosdb, Opts} when Type == counter ->
            exometer_report:subscribe(?MODULE, Name, value, ?INTERVAL(Opts, State));
        {kairosdb, Opts} when Type == histogram ->
            case proplists:get_value(datapoints, Opts) of
                undefined ->
                    lager:error("No datapoints are specified for metric ~p", [Name]);
                DPs ->
                    exometer_report:subscribe(?MODULE, Name, DPs, ?INTERVAL(Opts, State))
            end;
        false -> ok
    end,
    {ok, State};

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_Any, _State) ->
    ignore.

%% Internal functions
make_name(App, Name, DataPoint) ->
    case DataPoint of
        value -> %% Assume counter metric type
            list_to_binary([atom_to_list(App), ".", atom_to_list(Name)]);
        _ -> %% Assume histogram metric type
            list_to_binary([atom_to_list(App), ".", atom_to_list(Name), ".", atom_to_list(DataPoint)])
    end.

make_tags(Tags) ->
    make_tags(Tags, []).

make_tags([], Acc) -> Acc;
make_tags([Tag, Value | Rest], Acc) ->
    make_tags(Rest, [{Tag, Value} | Acc]).
