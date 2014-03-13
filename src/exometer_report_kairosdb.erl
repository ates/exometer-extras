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

-record(state, {
    url = ?DEFAULT_URL :: string(),
    tags = [] :: [proplists:property()]
}).

exometer_init(Options) ->
    URL = proplists:get_value(url, Options, ?DEFAULT_URL),
    Tags = proplists:get_value(tags, Options, []),
    {ok, #state{url = URL, tags = Tags}}.

exometer_report(Probe, DataPoint, _Extra, Value, State) ->
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

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_Any, _State) ->
    ignore.
