%%% @doc
%%% ==FLMQTT kube==
%%%  Kube configuration endpoint
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

-module(flmqtt_kube).

-export([config/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(PARAMS, [
	<<"name">>,
	<<"hw_type">>,
	<<"sw_version">>,
	<<"enable">>
]).

config(Dispatcher, Device, Jconfig) ->
	flmqtt_sql:execute(Dispatcher, kubes_clear, [timestamp(), Device]),
	[update(Dispatcher, Device, Kid, Config) ||
		{Kid, Config} <- cloudi_x_jsx:decode(Jconfig)].

update(Dispatcher, Device, Kid, Config) ->
	HwId = proplists:get_value(<<"hw_id">>, Config),
	Args = [proplists:get_value(Key, Config) || Key <- ?PARAMS],
	Args1 = Args ++ [Device, Kid, timestamp(), HwId],
	?LOG_DEBUG("~p kube config with args: ~p", [HwId, Args1]),
	update(Dispatcher, HwId, Args1).

update(_Dispatcher, undefined, _Args) ->
	{ok, section_ignored};
update(Dispatcher, _HwId, Args) ->
	insert(Dispatcher, Args, flmqtt_sql:execute(Dispatcher, kube_update, Args)).

insert(Dispatcher, [_, _, _, _, _, _, Config, _] = Args, {ok, no_update}) ->
	{ok, _} = flmqtt_sql:execute(Dispatcher, kube_insert, Args ++ [Config]),
	{ok, kube_inserted};
insert(_Dispatcher, _Args, {ok, _}) ->
	{ok, kube_updated}.

timestamp() ->
	{MegaSeconds, Seconds, _MicroSeconds} = now(),
	MegaSeconds * 1000000 + Seconds.

