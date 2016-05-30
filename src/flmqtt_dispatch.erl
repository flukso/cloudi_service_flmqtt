%%% @doc
%%% ==FLMQTT dispatch==
%%% This module implements the flmqtt_protocol behaviour to function
%%% as a central server endpoint to a Fluksometer. As such, the
%%% dispatcher deviates from the typical MQTT broker implementations.
%%% We're not trying to build a generic MQTT broker here, but terminate
%%% the bridge set up at the Fluksometer side to collect its published
%%% readings. At a later stage, device management functionality might be
%%% added to this dispatcher as well.
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2014 Sungjin Park <jinni.park@gmail.com>
%%%               2015 Bart Van Der Meerssche <bart@flukso.net>
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
%%% @author Sungjin Park <jinni.park@gmail.com>
%%% @author Bart Van Der Meerssche <bart@flukso.net>
%%% @copyright 2014 Sunjin Park
%%% @copyright 2015 Bart Van Der Meerssche
%%% @version 0.1.0 {@date} {@time}

-module(flmqtt_dispatch).
%-behavior(flmqtt_protocol).

%%
%% flmqtt_protocol callbacks
%%
-export([init/1, handle_message/2, handle_event/2, terminate/2]).

-include("props_to_record.hrl").
-include("flmqtt.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(KEEPALIVE_MULTIPLIER, 1500).
-define(TMPO_FORMAT, "/tmpo/sensor/~s/~s/~s/~s/~s").

-define(STATE_INIT, 0).
-define(STATE_SYNC, 1).
-define(STATE_LIVE, 2).

%% flmqtt_protocol context
-record(ctx, {
		device :: binary(),
		hardware :: integer(),
		state = ?STATE_INIT :: integer(),
		valid_keep_alive = {10, 900} :: {MinSec :: integer(), MaxSec :: integer()},
		timeout = 5000 :: timeout(),
		timestamp :: timestamp(),
		cloudi_dispatcher :: cloudi_service:dispatcher(),
		cloudi_dispatcher_ctx :: any(),
		cloudi_prefix :: list()
}).

-type context() :: #ctx{}.

-spec init(params()) -> {noreply, context(), timeout()}.
init(Params) ->
	Context = ?PROPS_TO_RECORD(Params, ctx),
	?LOG_DEBUG("initializing with ~p", [Context]),
	% TODO: Suboptimal to prepare statements for each conn init
	% but it'll do for now.
	flmqtt_sql:prepare(Context#ctx.cloudi_dispatcher),
	% Don't respond anything against tcp connection and apply small initial timeout.
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout}.

-spec handle_message(mqtt_message(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term()}.
handle_message(Message=#mqtt_connect{protocol= <<"MQIsdp">>, version=3, username=Device},
			Context=#ctx{state=?STATE_INIT}) ->
	?LOG_DEBUG("~p MSG IN ~p", [Device, Message]),
	accept(Message, Context);
handle_message(Message=#mqtt_connect{username=Device},
			Context=#ctx{state=?STATE_INIT})->
	?LOG_DEBUG("~p MSG IN ~p", [Device, Message]),
	Reply = flmqtt:connack([{code, incompatible}]),
	?LOG_DEBUG("~p MSG OUT ~p", [Device, Reply]),
	{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
handle_message(Message, Context=#ctx{state=?STATE_INIT}) ->
	% All the other messages are not allowed in STATE_INIT.
	?LOG_WARN("illegal MSG IN ~p", [Message]),
	{stop, normal, Context#ctx{timestamp=os:timestamp()}};
handle_message(Message=#mqtt_pingreq{}, Context) ->
	% Reflect ping and refresh timeout.
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	Reply = #mqtt_pingresp{},
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Reply]),
	{reply, Reply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_publish{topic=Topic, payload=Payload}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	% TODO reply PUBACK for QoS1 messages
	publish(re:split(Topic, "/"), Payload, Context);
handle_message(Message=#mqtt_puback{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubrec{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubrel{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubcomp{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_subscribe{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_unsubscribe{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_disconnect{}, Context) ->
	?LOG_DEBUG("~p MSG IN ~p", [Context#ctx.device, Message]),
	{stop, normal, Context#ctx{timestamp=os:timestamp()}};
handle_message(Message, Context) ->
	?LOG_WARN("~p unknown MSG IN ~p", [Context#ctx.device, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout}.

-spec handle_event(Event :: term(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term(), context()}.
handle_event(timeout, Context=#ctx{state=?STATE_SYNC}) ->
	sync(Context);
handle_event(timeout, Context=#ctx{device=Device}) ->
	% General timeout
	case Device of
		undefined -> ok;
		_ -> ?LOG_INFO("~p timed out", [Device])
	end,
	{stop, normal, Context};
handle_event({stop, From}, Context=#ctx{device=Device}) ->
	?LOG_INFO("~p stop signal from ~p", [Device, From]),
	{stop, normal, Context};
handle_event(Event, Context=#ctx{state=?STATE_INIT}) ->
	?LOG_ERROR("~p who sent this - ~p?", [Context#ctx.device,  Event]),
	{stop, normal, Context};
handle_event(Event=#mqtt_publish{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	case Event#mqtt_publish.dup of
		false ->
			case Event#mqtt_publish.qos of
				at_most_once -> todo; % mqtt_stat:transient(mqtt_publish_0_out);
				at_least_once -> todo;% mqtt_stat:transient(mqtt_publish_1_out);
				exactly_once -> todo; % mqtt_stat:transient(mqtt_publish_2_out);
				_ -> todo % mqtt_stat:transient(mqtt_publish_3_out)
			end;
		_ ->
			ok
	end,
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_puback{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubrec{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubrel{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubcomp{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_suback{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_unsuback{}, Context) ->
	?LOG_DEBUG("~p MSG OUT ~p", [Context#ctx.device, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event, Context) ->
	?LOG_WARN("~p unknown MSG OUT ~p", [Context#ctx.device, Event]),
	{noreply, Context, timeout(Context#ctx.timeout, Context#ctx.timestamp)}.

-spec terminate(Reason :: term(), context()) -> Reason :: term().
terminate(Reason, Context) ->
	?LOG_DEBUG("~p terminating with reason: ~p", [Context#ctx.device, Reason]),
	normal.

%%
%% Local Functions
%%
accept(Message=#mqtt_connect{username=Device, password=Key},
		Context=#ctx{cloudi_dispatcher=Dispatcher}) ->
	case flmqtt_auth:device(Dispatcher, Device, Key) of
		{ok, Hardware} ->
			?LOG_INFO("~p authorized", [Device]),
			KeepAlive = determine_keep_alive(
				Message#mqtt_connect.keep_alive,
				Context#ctx.valid_keep_alive),
			Timeout = KeepAlive*?KEEPALIVE_MULTIPLIER,
			Reply = flmqtt:connack([{code, accepted}]),
			?LOG_DEBUG("~p MSG OUT ~p", [Device, Reply]),
			{reply, Reply, Context#ctx{
				state = ?STATE_SYNC,
				device = Device,
				hardware = Hardware,
				timeout = Timeout,
				timestamp = os:timestamp()}, 0}; % time out instantly to start sync
		{error, not_found} ->
			?LOG_WARN("~p wrong username", [Device]),
			Reply = flmqtt:connack([{code, forbidden}]),
			?LOG_DEBUG("~p MSG OUT ~p", [Device, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
		{error, forbidden} ->
			?LOG_WARN("~p wrong password", [Device]),
			Reply = flmqtt:connack([{code, forbidden}]),
			?LOG_DEBUG("~p MSG OUT ~p", [Device, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
		{error, Error} ->
			?LOG_ERROR("~p error ~p in flmqtt_auth:device/3", [Device, Error]),
			Reply = flmqtt:connack([{code, unavailable}]),
			?LOG_DEBUG("~p MSG OUT ~p", [Device, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0}
	end.

sync(Context=#ctx{device=Device, cloudi_dispatcher=Dispatcher}) ->
	Topic = list_to_binary(["/d/device/", Device, "/tmpo/sync"]),
	Payload = flmqtt_tmpo:sync(Dispatcher, Device),
	?LOG_INFO("~p tx sync ~p with payload ~p", [Device, Topic, Payload]),
	Reply = flmqtt:publish([
		{topic, Topic},
		{payload, Payload}]),
	{reply, Reply, Context#ctx{
		state = ?STATE_LIVE,
		timestamp = os:timestamp()}, Context#ctx.timeout}.

publish([<<>>, <<"device">>, Device, <<"config">>, <<"kube">>], Payload,
	Context=#ctx{cloudi_dispatcher=Dispatcher, timeout=Timeout, device=Device}) ->
	?LOG_DEBUG("~p rx kube config update", [Device]),
	flmqtt_kube:config(Dispatcher, Device, Payload),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Timeout};
publish([<<>>, <<"device">>, Device, <<"config">>, <<"flx">>], Payload,
	Context=#ctx{cloudi_dispatcher=Dispatcher, timeout=Timeout, device=Device}) ->
	?LOG_DEBUG("~p rx flx/port config update", [Device]),
	flmqtt_port:config(Dispatcher, Device, Payload),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Timeout};
publish([<<>>, <<"device">>, Device, <<"config">>, <<"sensor">>], Payload,
		Context=#ctx{cloudi_dispatcher=Dispatcher, timeout=Timeout, device=Device}) ->
	?LOG_DEBUG("~p rx sensor config update", [Device]),
	flmqtt_sensor:config(Dispatcher, Device, Payload),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Timeout};
publish([<<>>, <<"device">>, Device, <<"tmpo">>, <<"sync">>], _Payload,
		Context=#ctx{device=Device}) ->
	?LOG_INFO("~p rx sync trigger from flm", [Device]),
	sync(Context);
publish([<<>>, <<"sensor">>, Sid, <<"tmpo">>, Rid, Lvl, Bid, Ext], Payload,
		Context=#ctx{cloudi_dispatcher=Dispatcher,timeout=Timeout,
		device=Device, hardware=Hardware}) ->
	case flmqtt_auth:sensor(Dispatcher, Sid, Device) of
		ok ->
			[RidInt, LvlInt, BidInt] =
				[list_to_integer(binary_to_list(X)) || X <- [Rid, Lvl, Bid]],
			flmqtt_tmpo:sink(Dispatcher, Sid, RidInt, LvlInt, BidInt, Ext, Payload),
			flmqtt_rrd:update(Hardware, Sid, LvlInt, Payload);
		_ ->
			?LOG_WARN("~p rx invalid tmpo sid ~p", [Device, Sid])
	end,
	{noreply, Context#ctx{timestamp=os:timestamp()}, Timeout};
publish(TopicList, _Payload, Context=#ctx{device=Device, timeout=Timeout}) ->
	?LOG_WARN("~p unrecognized flmqtt topic: ~p", [Device, TopicList]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Timeout}.

timeout(infinity, _) ->
	infinity;
timeout(Milliseconds, Timestamp) ->
	Elapsed = timer:now_diff(os:timestamp(), Timestamp) div 1000,
	case Milliseconds > Elapsed of
		true -> Milliseconds - Elapsed;
		_ -> 0
	end.

determine_keep_alive(Suggested, {Min, _}) when Suggested < Min ->
	Min;
determine_keep_alive(Suggested, {_, Max}) when Suggested > Max ->
	Max;
determine_keep_alive(Suggested, _) ->
	Suggested.

%%
%% Unit Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
