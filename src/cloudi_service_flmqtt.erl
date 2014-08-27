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
    Service = cloudi_service:self(Dispatcher),
    {ok, ListenerPid} = cloudi_x_ranch:start_listener(
        Service, % Ref
        100, % Number of acceptor processes
        cloudi_x_ranch_ssl, % Transport
        [{port, 8883}, % TransOpts
         {cacertfile, "/etc/ssl/certs/flukso.ca.crt"},
         {certfile, "/etc/ssl/certs/staging.flukso.net.crt"},
         {keyfile, "/etc/ssl/private/staging.flukso.net.key"},
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
