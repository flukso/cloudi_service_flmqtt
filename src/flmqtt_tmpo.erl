%%% @doc
%%% ==FLMQTT tmpo==
%%% Tmpo time series sink.
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

-module(flmqtt_tmpo).

-export([sink/7, sync/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(TMPO_MAX_BYTE_SIZE, 2097152). % 2MB
-define(GZ_MAGIC_NUMBER, 8075). % 0x1f8b

sink(Dispatcher, Sid, Rid, Lvl, Bid, Ext, Data) ->
	case check(timestamp(), blocksize(Lvl), Bid, Ext, magic(Data), byte_size(Data)) of
		ok ->
			case flmqtt_sql:execute(Dispatcher, tmpo_sink, 
					[Sid, Rid, Lvl, Bid, Ext, timestamp(), Data]) of
				{ok, _Result} ->
					clean(Dispatcher, Sid, Rid, Lvl, Bid, Ext),
					?LOG_INFO("~p tmpo block ~p/~p/~p sunk", [Sid, Rid, Lvl, Bid]),
					{ok, tmpo_file_sunk};
				{error, _Error} ->
					{error, tmpo_sink_sql_error}
			end;
		{error, Error} ->
			?LOG_WARN("~p rx tmpo error: ~p", [Sid, Error]),
			{error, Error}
	end.

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

check(_, _, _, _, _, Size) when Size > ?TMPO_MAX_BYTE_SIZE ->
	{error, tmpo_max_byte_size};
check(Timestamp, _, Bid, _, _, _) when Bid > Timestamp ->
	{error, tmpo_blocks_from_future};
check(_, Blocksize, Bid, _, _, _) when Bid rem Blocksize /= 0 ->
	{error, no_blocksize_multiple};
check(_, _, _, Ext, _, _) when Ext /= <<"gz">> ->
	{error, no_gz_compression};
check(_, _, _, <<"gz">>, Magic, _) when Magic /= ?GZ_MAGIC_NUMBER ->
	{error, no_gz_magic_in_payload};
check(_, _, _, _, _, _) ->
	ok.

last_child(Lvl, Bid) ->
	Bid + 15 * blocksize(Lvl - 4).

blocksize(Lvl) ->
	trunc(math:pow(2, Lvl)).

magic(<<>>) ->
	0;
magic(<<Magic:16, _/binary>>) ->
	Magic.

timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = now(),
    MegaSeconds * 1000000 + Seconds.

