%%% @doc
%%% ==FLMQTT sensor==
%%%  Sensor configuration endpoint
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2015 Bart Van Der Meerssche <bart@flukso.net>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @author Bart Van Der Meerssche <bart@flukso.net>
%%% @copyright 2015 Bart Van Der Meerssche
%%% @version 0.1.0 {@date} {@time}

-module(flmqtt_sensor).

-export([config/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(PARAMS, [
	<<"type">>,
	<<"class">>,
	<<"function">>,
	<<"voltage">>,
	<<"current">>,
	<<"constant">>,
	<<"kid">>,
	<<"rid">>,
	<<"data_type">>,
	<<"enabled">>
%	<<"port">>, % see port()
]).

config(Dispatcher, Device, Jconfig) ->
	{ok, Result} = flmqtt_sql:execute(Dispatcher, sensors, [Device]),
	Sids = [{Sid, true} || [Sid] <- Result],
	SidsDict = orddict:from_list(Sids),
	[update(Dispatcher, SidsDict, Config) ||
		{_Sidx, Config} <- cloudi_x_jsx:decode(Jconfig)].

update(Dispatcher, SidsDict, Config) ->
	Sid = proplists:get_value(<<"id">>, Config),
	% filters out non-sensor entries in the config as well
	update(Dispatcher, Sid, Config, orddict:is_key(Sid, SidsDict)).

update(Dispatcher, Sid, Config, true) ->
	Args = [proplists:get_value(Key, Config) || Key <- ?PARAMS],
	Port = to_json(proplists:get_value(<<"port">>, Config)),
	Args1 = Args ++ [Port, timestamp(), Sid],
	?LOG_DEBUG("~p sensor config with args: ~p", [Sid, Args1]),
	{ok, _} = flmqtt_sql:execute(Dispatcher, sensor_config, Args1),
	flmqtt_rrd:create(Sid, proplists:get_value(<<"class">>, Config)),
	{ok, sensor_updated};
update(_Dispatcher, _Sid, _Config, false) ->
	{error, sensor_not_found}.

to_json(undefined) ->
	undefined;
to_json(X) ->
	cloudi_x_jsx:encode(X).

timestamp() ->
	{MegaSeconds, Seconds, _MicroSeconds} = now(),
	MegaSeconds * 1000000 + Seconds.

