%%% @doc
%%% ==FLMQTT tmpo==
%%% Tmpo time series sink.
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2014 Bart Van Der Meerssche <bart@flukso.net>
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
%%% @copyright 2014 Bart Van Der Meerssche
%%% @version 0.1.0 {@date} {@time}

-module(flmqtt_tmpo).

-export([sink/7, sync/2]).

sink(Dispatcher, Sid, Rid, Lvl, Bid, Ext, Data) ->
	{ok, _Result} = flmqtt_sql:execute(Dispatcher, tmpo_sink, 
		[Sid, Rid, Lvl, Bid, Ext, timestamp(), Data]),
	clean(Dispatcher, Sid, Rid, Lvl, Bid, Ext),
	{ok, tmpo_file_sunk}.

sync(Dispatcher, Device) ->
	{ok, Active} = flmqtt_sql:execute(Dispatcher, active_sensors, [Device]),
	cloudi_x_jsx:encode([sync_sensor(Dispatcher, Sid) || [Sid] <- Active]).

sync_sensor(Dispatcher, Sid) ->
	case flmqtt_sql:execute(Dispatcher, tmpo_last, [Sid]) of
		{ok, []} ->
			[{sid, Sid}, {rid, 0}, {lvl, 0}, {bid, 0}];
		{ok, [[_Sid, Rid, Lvl, Bid, Ext]]} ->
			clean(Dispatcher, Sid, Rid, Lvl, Bid, Ext),
			[{sid, Sid}, {rid, Rid}, {lvl, Lvl}, {bid, Bid}]
	end.

clean(Dispatcher, Sid, Rid, Lvl, Bid, Ext) when Lvl > 8 ->
	LastChild = last_child(Lvl, Bid),
	{ok, _Result} = flmqtt_sql:execute(Dispatcher, tmpo_clean,
		[Sid, Rid, Lvl - 4, LastChild, Ext]),
	clean(Dispatcher, Sid, Rid, Lvl - 4, LastChild, Ext),
	{ok, tmpo_block_cleaning_done};
clean(_,_, _, _, _, _) ->
	{ok, no_tmpo_block_cleaning_needed}.

last_child(Lvl, Bid) ->
	Delta = trunc(math:pow(2, Lvl - 4)),
	Bid + 15 * Delta.

timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = now(),
    MegaSeconds * 1000000 + Seconds.

