%%% @doc
%%% ==Cloudi Service FLMQTT==
%%% A Cloudi endpoint for syncing Fluksometer sensor readings over MQTT.
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

-module(cloudi_service_flmqtt).
-behaviour(cloudi_service).

% cloudi service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").

-record(state, {
        listener,
        service
    }).

cloudi_service_init(_Args, Prefix, Dispatcher) ->
    flmqtt_sql:prepare(Dispatcher),
    Service = cloudi_service:self(Dispatcher),
    {ok, ListenerPid} = cloudi_x_ranch:start_listener(
        Service, % Ref
        100, % Number of acceptor processes
        cloudi_x_ranch_ssl, % Transport
        [{port, 8883}, % TransOpts
         {cacertfile, "/etc/ssl/certs/flukso.ca.crt"},
         {certfile, "/etc/ssl/certs/api.flukso.net.crt"},
         {keyfile, "/etc/ssl/private/api.flukso.net.key"},
         {verify, verify_none},
         {max_connections, unlimited}],
        flmqtt_protocol, % Protocol
        [{cloudi_dispatcher, cloudi_service:dispatcher(Dispatcher)}, % ProtoOpts
         {cloudi_dispatcher_ctx, create_context(Dispatcher)}, % cf cloudi_service_children.hrl
         {cloudi_prefix, Prefix}]
    ),
    {ok, #state{listener = ListenerPid,
                service = Service}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{service = Service}) ->
    cloudi_x_ranch:stop_listener(Service),
    ok.
