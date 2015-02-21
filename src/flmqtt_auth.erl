%%% @doc
%%% ==FLMQTT authentication and authorisation==
%%% Fluksometer MQTT authentication/authorisation module.
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

-module(flmqtt_auth).

-export([device/3, sensor/3]).

device(Dispatcher, Device, Key) ->
	verify(flmqtt_sql:execute(Dispatcher, auth_device, [Device]), Key).

sensor(Dispatcher, Sensor, Device) ->
	verify(flmqtt_sql:execute(Dispatcher, auth_sensor, [Sensor]), Device).

verify({ok, []}, _Credential) ->
	{error, not_found};
verify({ok, [[Credential]]}, Credential) ->
	ok;
verify({ok, _Result}, _Credential) ->
	{error, forbidden};
verify({error, Error}, _Credential) ->
	{error, Error}.

