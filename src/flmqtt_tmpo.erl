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

-export([sink/7]).

sink(Dispatcher, Sid, Rid, Lvl, Bid, Ext, Data) ->
	[RidInt, LvlInt, BidInt] =
		[list_to_integer(binary_to_list(X)) || X <- [Rid, Lvl, Bid]],
	{ok, _Result} = flmqtt_sql:execute(Dispatcher, tmpo_sink, 
		[Sid, RidInt, LvlInt, BidInt, Ext, timestamp(), Data]),
	clean(Dispatcher, Sid, RidInt, LvlInt, Ext),
	{ok, tmpo_file_sunk}.

clean(Dispatcher, Sid, RidInt, LvlInt, Ext) when LvlInt > 8 ->
	{ok, _Result} = flmqtt_sql:execute(Dispatcher, tmpo_clean,
		[Sid, RidInt, LvlInt - 4, Ext]),
	{ok, tmpo_block_cleaning_done};
clean(_, _, _, _, _) ->
	{ok, no_tmpo_block_cleaning_needed}.

children(LvlInt, BidInt) ->
	Delta = trunc(math:pow(2, LvlInt - 4)),
	[integer_to_list(BidInt + Pos * Delta) || Pos <- lists:seq(0, 15)].

timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = now(),
    MegaSeconds * 1000000 + Seconds.

