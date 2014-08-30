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

-export([sink/6]).

-define(TMPO_TMP_DIR, <<"/var/run/flukso/tmpo/tmp/">>).
-define(TMPO_ROOT_DIR, <<"/var/run/flukso/tmpo/sensor/">>).

sink(Sid, Rid, Lvl, Bid, Ext, Payload) ->
	TmpPath = tmppath(Sid, Rid, Lvl, Bid, Ext),
	{ok, Fd} = file:open(TmpPath, [write]),
	ok = file:write(Fd, Payload),
	ok = file:datasync(Fd),
	ok = file:close(Fd),
	Path = path(Sid, Rid, Lvl, Bid, Ext),
	ok = filelib:ensure_dir(Path),
	ok = file:rename(TmpPath, Path),
	LvlInt = list_to_integer(binary_to_list(Lvl)),
	BidInt = list_to_integer(binary_to_list(Bid)),
	clean(Sid, Rid, LvlInt, BidInt, Ext),
	{ok, tmpo_file_sunk}.

clean(Sid, Rid, LvlInt, BidInt, Ext) when LvlInt > 8 ->
	Children = children(LvlInt, BidInt),
	ChildLvl = integer_to_list(LvlInt - 4),
	[file:delete(path(Sid, Rid, ChildLvl, Child, Ext)) || Child <- Children],
	{ok, tmpo_block_cleaning_done};
clean(_, _, _, _, _) ->
	{ok, no_tmpo_block_cleaning_needed}.

children(LvlInt, BidInt) ->
	Delta = trunc(math:pow(2, LvlInt - 4)),
	[integer_to_list(BidInt + Pos * Delta) || Pos <- lists:seq(0, 15)].

tmppath(Sid, Rid, Lvl, Bid, Ext) ->
	iolist_to_binary([?TMPO_TMP_DIR, Sid, "-", Rid, "-", Lvl, "-", Bid, ".", Ext]).

path(Sid, Rid, Lvl, Bid, Ext) ->
	iolist_to_binary([?TMPO_ROOT_DIR, Sid, "/", Rid, "/", Lvl, "/", Bid, ".", Ext]).

