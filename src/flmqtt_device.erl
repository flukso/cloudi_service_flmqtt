%%% @doc
%%% ==flmqtt device==
%%%  device configuration endpoint
%%% @end
%%%
%%% mit license
%%%
%%% copyright (c) 2017 bart van der meerssche <bart@flukso.net>
%%%
%%% permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "software"), to deal
%%% in the software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the software, and to permit persons to whom the software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% the above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the software.
%%%
%%% the software is provided "as is", without warranty of any kind, express or
%%% implied, including but not limited to the warranties of merchantability,
%%% fitness for a particular purpose and noninfringement. in no event shall the
%%% authors or copyright holders be liable for any claim, damages or other
%%% liability, whether in an action of contract, tort or otherwise, arising from,
%%% out of or in connection with the software or the use or other dealings in
%%% the software.
%%%
%%% @author bart van der meerssche <bart@flukso.net>
%%% @copyright 2015 Bart Van Der Meerssche
%%% @version 0.1.0 {@date} {@time}

-module(flmqtt_device).

-export([sink_tap/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

sink_tap(Dispatcher, Device, Payload) ->
	flmqtt_sql:execute(Dispatcher, device_sink_tap, [Payload, Device]).

