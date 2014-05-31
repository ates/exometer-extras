-module(exometer_report_snmpd).

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

-define(DEFAULT_INTERVAL, 10000). %% 10 seconds
-define(INTERVAL(Opts, State), proplists:get_value(interval, Opts, State#state.interval)).

-record(state, {idx = 0 :: non_neg_integer(), interval = ?DEFAULT_INTERVAL :: pos_integer()}).

exometer_init(Options) ->
    MIBPath = proplists:get_value(mib_template, Options),
    MIBWorkPath = proplists:get_value(mib_dir, Options),
    MIBPath1 = filename:join([MIBWorkPath, filename:basename(MIBPath)]),
    {ok, _} = file:copy(MIBPath, MIBPath1),
    ok = load_mib(MIBPath1),
    ?MODULE = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    {ok, #state{idx = 0, interval = Interval}}.

exometer_report(Metric, _DataPoint, _Extra, Value, State) ->
    case ets:lookup(?MODULE, Metric) of
        [{Metric, Idx}] ->
            Name = string:join([atom_to_list(N) || N <- Metric], "_"),
            snmpa_local_db:table_create_row(appStats, [Idx], {Name, Value});
        _ -> ok
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

exometer_newentry(#exometer_entry{name = Name, type = Type, options = Options}, #state{idx = Idx} = State) ->
    case lists:keyfind(snmp, 1, Options) of
        {snmp, Opts} when Type == counter ->
            true = ets:insert(?MODULE, {Name, State#state.idx}),
            exometer_report:subscribe(?MODULE, Name, value, ?INTERVAL(Opts, State));
        _ -> ok %% the only counter type is supported
    end,
    {ok, State#state{idx = Idx + 1}};

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_Any, _State) ->
    ignore.

%% Internal functions
load_mib(MIB) ->
    case snmpc:compile(MIB, [{outdir, filename:dirname(MIB)}]) of
        {ok, MibBin0} ->
            MibBin1 = filename:rootname(MibBin0),
            case snmpa:load_mibs(snmp_master_agent, [MibBin1]) of
                ok -> ok;
                {error, Reason} ->
                    lager:error("Can't load MIB ~p: ~p", [MIB, Reason]),
                    {error, Reason}
            end;
        {error, Reason} -> 
            lager:error("Can't compile MIB: ~p: ~p", [MIB, Reason]),
            {error, Reason}
    end.
