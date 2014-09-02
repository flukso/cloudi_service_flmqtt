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
%%%                    Bart Van Der Meerssche <bart@flukso.net>
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
%%% @copyright 2014 Bart Van Der Meerssche
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

%% flmqtt_protocol context
-record(ctx, {
		client_id :: binary(),
		auth = flmqtt_auth :: module(),
		session = false :: boolean(),
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
	% Don't respond anything against tcp connection and apply small initial timeout.
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout}.

-spec handle_message(mqtt_message(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term()}.
handle_message(Message=#mqtt_connect{protocol= <<"MQIsdp">>, version=3, client_id=ClientId},
			   Context=#ctx{session=false}) ->
	?LOG_INFO("~p MSG IN ~p", [ClientId, Message]),
	accept(Message, Context);
handle_message(Message=#mqtt_connect{client_id=ClientId},
				Context=#ctx{session=false})->
	?LOG_INFO("~p MSG IN ~p", [ClientId, Message]),
	Reply = flmqtt:connack([{code, incompatible}]),
	?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
	{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
handle_message(Message, Context=#ctx{session=false}) ->
	% All the other messages are not allowed without session.
	?LOG_WARN("illegal MSG IN ~p", [Message]),
	{stop, normal, Context#ctx{timestamp=os:timestamp()}};
handle_message(Message=#mqtt_pingreq{}, Context) ->
	% Reflect ping and refresh timeout.
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	Reply = #mqtt_pingresp{},
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Reply]),
	{reply, Reply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_publish{topic=Topic, qos=_Qos, payload=Payload}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	publish(re:split(Topic, "/"), Payload, Context),
	% TODO reply PUBACK for QoS1 messages
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_puback{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubrec{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubrel{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_pubcomp{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_subscribe{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_unsubscribe{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_message(Message=#mqtt_disconnect{}, Context) ->
	?LOG_INFO("~p MSG IN ~p", [Context#ctx.client_id, Message]),
	{stop, normal, Context#ctx{timestamp=os:timestamp()}};
handle_message(Message, Context) ->
	?LOG_WARN("~p unknown MSG IN ~p", [Context#ctx.client_id, Message]),
	{noreply, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout}.

-spec handle_event(Event :: term(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term(), context()}.
handle_event(timeout, Context=#ctx{client_id=ClientId}) ->
	% General timeout
	case ClientId of
		undefined -> ok;
		_ -> ?LOG_INFO("~p timed out", [ClientId])
	end,
	{stop, normal, Context};
handle_event({stop, From}, Context=#ctx{client_id=ClientId}) ->
	?LOG_DEBUG("~p stop signal from ~p", [ClientId, From]),
	{stop, normal, Context};
handle_event(Event, Context=#ctx{session=false}) ->
	?LOG_ERROR("~p who sent this - ~p?", [Context#ctx.client_id,  Event]),
	{stop, normal, Context};
handle_event(Event=#mqtt_publish{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
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
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubrec{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubrel{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_pubcomp{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_suback{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event=#mqtt_unsuback{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#ctx.client_id, Event]),
	{reply, Event, Context#ctx{timestamp=os:timestamp()}, Context#ctx.timeout};
handle_event(Event, Context) ->
	?LOG_WARN("~p unknown MSG OUT ~p", [Context#ctx.client_id, Event]),
	{noreply, Context, timeout(Context#ctx.timeout, Context#ctx.timestamp)}.

-spec terminate(Reason :: term(), context()) -> Reason :: term().
terminate(Reason, Context) ->
	?LOG_DEBUG("~p terminating", [Context#ctx.client_id]),
	case Reason of
		#mqtt_publish{} -> Context#ctx.session ! {recover, Reason};
		#mqtt_puback{} -> Context#ctx.session ! {recover, Reason};
		_ -> ok
	end,
	normal.

%%
%% Local Functions
%%
accept(Message=#mqtt_connect{client_id=ClientId, username=Username, password=Password},
		Context=#ctx{auth=Auth}) ->
	case Auth:verify(Username, Password) of
		ok ->
			?LOG_DEBUG("~p authorized ~p", [ClientId, Username]),
			KeepAlive = determine_keep_alive(Message#mqtt_connect.keep_alive, Context#ctx.valid_keep_alive),
			Timeout = KeepAlive*?KEEPALIVE_MULTIPLIER,
			Reply = flmqtt:connack([{code, accepted}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply,
			 Context#ctx{session=true, client_id=ClientId, timeout=Timeout, timestamp=os:timestamp()}, Timeout};
		{error, not_found} ->
			?LOG_WARN("~p wrong username ~p", [ClientId, Username]),
			Reply = flmqtt:connack([{code, forbidden}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
		{error, forbidden} ->
			?LOG_WARN("~p wrong password ~p", [ClientId, Username]),
			Reply = flmqtt:connack([{code, forbidden}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0};
		Error ->
			?LOG_ERROR("~p error ~p in ~p:verify/2", [ClientId, Error, Auth]),
			Reply = flmqtt:connack([{code, unavailable}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#ctx{timestamp=os:timestamp()}, 0}
	end.

publish([<<>>, <<"sensor">>, Sid, <<"tmpo">>, Rid, Lvl, Bid, Ext], Payload, Context) ->
	Dispatcher = Context#ctx.cloudi_dispatcher,
	flmqtt_tmpo:sink(Dispatcher, Sid, Rid, Lvl, Bid, Ext, Payload);
publish(TopicList, _Payload, _Context) ->
	?LOG_WARN("unrecognized flmqtt topic: ~p", [TopicList]).

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
