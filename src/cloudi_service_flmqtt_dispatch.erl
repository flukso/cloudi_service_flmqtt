%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%           Bart Van Der Meerssche <bart@flukso.net>
%%%
%%% Description : MQTT dispatcher.
%%%    This module implements the mqtt_protocol behaviour to function
%%% as a central server endpoint to a Fluksometer. As such, the
%%% dispatcher deviates from the typical MQTT broker implementations.
%%% We're not trying to build a generic MQTT broker here, but terminate
%%% the bridges set up at the Fluksometer side to collect the readings
%%% published on the bridges. At a later stage, device management
%%% functionality might be added to this dispatcher as well.
%%%
%%% Created : Nov 14, 2012
%%% Trimmed : Jul 17, 2014
%%% -------------------------------------------------------------------
-module(cloudi_service_flmqtt_dispatch).
-author("Bart Van Der Meerssche <bart@flukso.net>").
%-behavior(mqtt_protocol).

%%
%% mqtt_protocol callbacks
%%
-export([init/1, handle_message/2, handle_event/2, terminate/2]).

-include("mqtt.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(KEEPALIVE_MULTIPLIER, 1500).
-define(CONTEXT, ?MODULE).

%% mqtt_protocol context
-record(?CONTEXT, {
		client_id :: binary(),
		auth = cloudi_service_flmqtt_auth :: module(),
		session :: pid(),
		valid_keep_alive = {10, 900} :: {MinSec :: integer(), MaxSec :: integer()},
		timeout = 5000 :: timeout(),
		timestamp :: timestamp()
}).

-type context() :: #?CONTEXT{}.

-spec init(params()) -> {noreply, context(), timeout()}.
init(Params) ->
	%Default = ?PROPS_TO_RECORD(fubar:settings(?MODULE), ?CONTEXT),
	%Context = ?PROPS_TO_RECORD(Params, ?CONTEXT, Default)(),
	Context = #?CONTEXT{},
	?LOG_DEBUG("initializing with ~p", [Context]),
	% Don't respond anything against tcp connection and apply small initial timeout.
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}.

-spec handle_message(mqtt_message(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term()}.
handle_message(Message=#mqtt_connect{protocol= <<"MQIsdp">>, version=3, client_id=ClientId},
			   Context=#?CONTEXT{session=undefined}) ->
	?LOG_INFO("~p MSG IN ~p", [ClientId, Message]),
	accept(Message, Context);
handle_message(Message=#mqtt_connect{client_id=ClientId},
				Context=#?CONTEXT{session=undefined})->
	?LOG_INFO("~p MSG IN ~p", [ClientId, Message]),
	Reply = mqtt:connack([{code, incompatible}]),
	?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
	{reply, Reply, Context#?CONTEXT{timestamp=os:timestamp()}, 0};
handle_message(Message, Context=#?CONTEXT{session=undefined}) ->
	% All the other messages are not allowed without session.
	?LOG_WARN("illegal MSG IN ~p", [Message]),
	{stop, normal, Context#?CONTEXT{timestamp=os:timestamp()}};
handle_message(Message=#mqtt_pingreq{}, Context) ->
	% Reflect ping and refresh timeout.
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Reply = #mqtt_pingresp{},
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Reply]),
	{reply, Reply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_publish{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	case Message#mqtt_publish.dup of
		false ->
			case Message#mqtt_publish.qos of
				at_most_once -> todo; % mqtt_stat:transient(mqtt_publish_0_in);
				at_least_once -> todo; % mqtt_stat:transient(mqtt_publish_1_in);
				exactly_once -> todo; % mqtt_stat:transient(mqtt_publish_2_in);
				_ -> todo % mqtt_stat:transient(mqtt_publish_3_in)
			end;
		_ ->
			ok
	end,
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_puback{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_pubrec{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_pubrel{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_pubcomp{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_subscribe{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_unsubscribe{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	Session ! Message,
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_message(Message=#mqtt_disconnect{}, Context=#?CONTEXT{session=Session}) ->
	?LOG_INFO("~p MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	mqtt_session:stop(Session),
	{stop, normal, Context#?CONTEXT{timestamp=os:timestamp()}};
handle_message(Message, Context) ->
	?LOG_WARN("~p unknown MSG IN ~p", [Context#?CONTEXT.client_id, Message]),
	{noreply, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout}.

-spec handle_event(Event :: term(), context()) ->
		  {reply, mqtt_message(), context(), timeout()} |
		  {noreply, context(), timeout()} |
		  {stop, Reason :: term(), context()}.
handle_event(timeout, Context=#?CONTEXT{client_id=ClientId}) ->
	% General timeout
	case ClientId of
		undefined -> ok;
		_ -> ?LOG_INFO("~p timed out", [ClientId])
	end,
	{stop, normal, Context};
handle_event({stop, From}, Context=#?CONTEXT{client_id=ClientId}) ->
	?LOG_DEBUG("~p stop signal from ~p", [ClientId, From]),
	{stop, normal, Context};
handle_event(Event, Context=#?CONTEXT{session=undefined}) ->
	?LOG_ERROR("~p who sent this - ~p?", [Context#?CONTEXT.client_id,  Event]),
	{stop, normal, Context};
handle_event(Event=#mqtt_publish{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
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
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_puback{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_pubrec{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_pubrel{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_pubcomp{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_suback{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event=#mqtt_unsuback{}, Context) ->
	?LOG_INFO("~p MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{reply, Event, Context#?CONTEXT{timestamp=os:timestamp()}, Context#?CONTEXT.timeout};
handle_event(Event, Context) ->
	?LOG_WARN("~p unknown MSG OUT ~p", [Context#?CONTEXT.client_id, Event]),
	{noreply, Context, timeout(Context#?CONTEXT.timeout, Context#?CONTEXT.timestamp)}.

-spec terminate(Reason :: term(), context()) -> Reason :: term().
terminate(Reason, Context) ->
	?LOG_DEBUG("~p terminating", [Context#?CONTEXT.client_id]),
	case Reason of
		#mqtt_publish{} -> Context#?CONTEXT.session ! {recover, Reason};
		#mqtt_puback{} -> Context#?CONTEXT.session ! {recover, Reason};
		_ -> ok
	end,
	normal.

%%
%% Local Functions
%%
accept(Message=#mqtt_connect{client_id=ClientId, username=Username, password=Password},
		Context=#?CONTEXT{auth=Auth}) ->
	case Auth:verify(Username, Password) of
		ok ->
			?LOG_DEBUG("~p authorized ~p", [ClientId, Username]),
			KeepAlive = determine_keep_alive(Message#mqtt_connect.keep_alive, Context#?CONTEXT.valid_keep_alive),
			Timeout = KeepAlive*?KEEPALIVE_MULTIPLIER,
			Reply = mqtt:connack([{code, accepted}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply,
			 Context#?CONTEXT{client_id=ClientId, timeout=Timeout, timestamp=os:timestamp()}, Timeout};
		{error, not_found} ->
			?LOG_WARN("~p wrong username ~p", [ClientId, Username]),
			Reply = mqtt:connack([{code, forbidden}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#?CONTEXT{timestamp=os:timestamp()}, 0};
		{error, forbidden} ->
			?LOG_WARN("~p wrong password ~p", [ClientId, Username]),
			Reply = mqtt:connack([{code, forbidden}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#?CONTEXT{timestamp=os:timestamp()}, 0};
		Error ->
			?LOG_ERROR("~p error ~p in ~p:verify/2", [ClientId, Error, Auth]),
			Reply = mqtt:connack([{code, unavailable}]),
			?LOG_INFO("~p MSG OUT ~p", [ClientId, Reply]),
			{reply, Reply, Context#?CONTEXT{timestamp=os:timestamp()}, 0}
	end.

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
