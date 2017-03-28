%%% @doc
%%% ==FLMQTT port==
%%%  Port configuration endpoint
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2016 Bart Van Der Meerssche <bart@flukso.net>
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
%%% @copyright 2016 Bart Van Der Meerssche
%%% @version 0.1.0 {@date} {@time}

-module(flmqtt_port).

-export([config/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(PARAMS, [
	<<"name">>,
	<<"class">>,
	<<"current">>,
	<<"trigger">>,
	<<"constant">>,
	<<"dsmr">>,
	<<"enable">>
]).

config(Dispatcher, Device, Jconfig) ->
	[update(Dispatcher, Device, Port, Config) ||
		{Port, Config} <- cloudi_x_jsx:decode(Jconfig)].

update(Dispatcher, Device, Port, Config) ->
	Args = [proplists:get_value(Key, Config) || Key <- ?PARAMS],
	Args1 = Args ++ [timestamp(), Device, Port],
	?LOG_DEBUG("~p port config with args: ~p", [[Device, Port], Args1]),
	update(Dispatcher, proplists:get_value(<<"enable">>, Config), Args1).

update(_Dispatcher, undefined, _Args) ->
	{ok, section_ignored};
update(Dispatcher, _Enable, [Name, _, _, _, _, _, _, _, Device, Port] = Args) ->
	insert(Dispatcher, Args, flmqtt_sql:execute(Dispatcher, port_update, Args)),
	update_sensor_compat(Dispatcher, Device, Port, Name).

insert(Dispatcher, [_, _, _, _, _, _, _, Config, _, _] = Args, {updated, 0}) ->
	{updated, 1} = flmqtt_sql:execute(Dispatcher, port_insert, Args ++ [Config]),
	{ok, port_inserted};
insert(_Dispatcher, _Args, {updated, 1}) ->
	{ok, port_updated}.

update_sensor_compat(Dispatcher, Device, Port, Name) when Port < <<"7">> ->
	Args = [Name, Device, list_to_binary(["[", Port, "]"])],
	flmqtt_sql:execute(Dispatcher, sensor_compat, Args),
	{ok, sensor_compat_updated};
update_sensor_compat(_Dispatcher, _Device, _Port, _Name) ->
	{ok, sensor_compat_ignored}.

timestamp() ->
	{MegaSeconds, Seconds, _MicroSeconds} = now(),
	MegaSeconds * 1000000 + Seconds.

