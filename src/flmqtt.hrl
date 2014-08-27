%%% @doc
%%% ==FLMQTT hrl==
%%% Contains FLMQTT record definitions 
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2012 Sungjin Park <jinni.park@gmail.com>
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
%%% @copyright 2012 Sungjin Park
%%% @version 0.1.0 {@date} {@time}

-type hostname() :: string().
-type ipaddr() :: ipv4addr() | ipv6addr().
-type ipv4addr() :: {byte(),byte(),byte(),byte()}.
-type ipv6addr() :: {word(),word(),word(),word(),word(),word(),word(),word()}.
-type ipport() :: word().
-type nwaddr() :: {hostname(),ipport()} | {ipaddr(),ipport()}.
-type socket() :: port().
-type param() :: {atom(), term()}.
-type proplist(Key, Value) :: [{Key, Value} | Key].
-type params() :: proplist(atom(), term()).
-type reason() :: term().
-type word() :: 0..65535.
-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-type mqtt_command() :: mqtt_reserved |
		  mqtt_connect | mqtt_connack |
		  mqtt_publish | mqtt_puback | mqtt_pubrec | mqtt_pubrel | mqtt_pubcomp |
		  mqtt_subscribe | mqtt_suback | mqtt_unsubscribe | mqtt_unsuback |
		  mqtt_pingreq | mqtt_pingresp |
		  mqtt_disconnect.
-type mqtt_qos() :: at_most_once | at_least_once | exactly_once.

-record(mqtt_header, {type :: mqtt_command(),
					  dup = false :: boolean(),
					  qos = at_most_once :: mqtt_qos(),
					  retain = false :: boolean(),
					  size :: undefined | non_neg_integer()}).

-record(mqtt_reserved, {dup = false :: boolean(),
						qos = at_most_once :: mqtt_qos(),
						retain = false :: boolean(),
						extra = <<>> :: binary()}).

-record(mqtt_connect, {protocol = <<"MQIsdp">> :: binary(),
					   version = 3 :: pos_integer(),
					   username = <<>> :: binary(),
					   password = <<>> :: binary(),
					   will_retain = false :: boolean(),
					   will_qos = at_most_once :: mqtt_qos(),
					   will_topic :: undefined | binary(),
					   will_message :: undefined | binary(),
					   clean_session = false,
					   keep_alive = 600 :: pos_integer(),
					   client_id = <<>> :: binary(),
					   max_recursion = 0 :: integer(),
					   extra = <<>> :: binary()}).

-type mqtt_connack_code() :: accepted | incompatible | id_rejected | unavailable |
		forbidden | unauthorized | alt_server.

-record(mqtt_connack, {code = accepted :: mqtt_connack_code(),
					   alt_server = <<>> :: binary(),
					   max_recursion = 0 :: integer(),
					   extra = <<>> :: binary()}).

-record(mqtt_publish, {topic = <<>> :: binary(),
					   message_id :: undefined | pos_integer(),
					   dup = false:: boolean(),
					   qos = at_most_once :: mqtt_qos(),
					   retain = false :: boolean(),
					   payload = <<>> :: binary()}).

-record(mqtt_puback, {message_id :: pos_integer(),
					  extra = <<>> :: binary()}).

-record(mqtt_pubrec, {message_id :: pos_integer(),
					  extra = <<>> :: binary()}).

-record(mqtt_pubrel, {message_id :: pos_integer(),
					  extra = <<>> :: binary()}).

-record(mqtt_pubcomp, {message_id :: pos_integer(),
					   extra = <<>> :: binary()}).

-record(mqtt_subscribe, {message_id :: pos_integer(),
						 topics = [] :: [{binary(), mqtt_qos()}],
						 dup = false :: boolean(),
						 qos = at_least_once :: mqtt_qos(),
						 extra = <<>> :: binary()}).

-record(mqtt_suback, {message_id :: pos_integer(),
					  qoss = [] :: [mqtt_qos()],
					  extra = <<>> :: binary()}).

-record(mqtt_unsubscribe, {message_id :: pos_integer(),
						   topics = [] :: [binary()],
						   dup = false :: boolean(),
						   qos = at_least_once :: mqtt_qos(),
						   extra = <<>> :: binary()}).

-record(mqtt_unsuback, {message_id :: pos_integer(),
						extra = <<>> :: binary()}).

-record(mqtt_pingreq, {extra = <<>> :: binary()}).

-record(mqtt_pingresp, {extra = <<>> :: binary()}).

-record(mqtt_disconnect, {extra = <<>> :: binary()}).

-type mqtt_message() :: #mqtt_reserved{} | #mqtt_connect{} | #mqtt_connack{} |
		#mqtt_publish{} | #mqtt_puback{} | #mqtt_pubrec{} | #mqtt_pubrel{} | #mqtt_pubcomp{} |
		#mqtt_subscribe{} | #mqtt_suback{} | #mqtt_unsubscribe{} | #mqtt_unsuback{} |
		#mqtt_pingreq{} | #mqtt_pingresp{} |
		#mqtt_disconnect{}.
