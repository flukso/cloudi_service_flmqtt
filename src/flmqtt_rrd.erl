%%% @doc
%%% ==FLMQTT rrd==
%%%  Rrd library functions
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

-module(flmqtt_rrd).

-export([create/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(FLUKSO_UID, 1000).
-define(FLUKSO_GID, 1000).
-define(FLUKSO_PATH_DATA , "/var/lib/flukso/data/").
-define(RRD_PATH_BASE, [?FLUKSO_PATH_DATA, "base/"]).
-define(RRD_PATH_NIGHT, [?FLUKSO_PATH_DATA, "night/"]).
-define(RRD_CREATE_CLASSES, [
	{<<"analog">>, true},
	{<<"pulse">>, true},
	{<<"cosem">>, true}
]).

create(Sid, Class) ->
	create(Sid, proplists:get_value(Class, ?RRD_CREATE_CLASSES),
		file:read_file_info(path(base, Sid))).

create(Sid, true, {error, enoent}) ->
	CmdBase = join([
		"rrdtool create",
		path(base, Sid),
		"-b 1199487600",
		"-s 60",
		"DS:meter:DERIVE:8640000:-20:20",
		"RRA:AVERAGE:0.5:1:1440",
		"RRA:AVERAGE:0.5:15:672",
		"RRA:AVERAGE:0.5:1440:365",
		"RRA:AVERAGE:0.5:10080:520"
	], " "),
	?LOG_DEBUG("rrdcreate base command: ~p", [CmdBase]),
	os:cmd(CmdBase),
	file:change_owner(path(base, Sid), ?FLUKSO_UID, ?FLUKSO_GID),
	CmdNight = join([
		"rrdtool create",
		path(night, Sid),
		"-b 1199487600",
		"-s 86400",
		"DS:meter:GAUGE:8640000:-20:20",
		"RRA:AVERAGE:0.5:1:365",
		"RRA:AVERAGE:0.5:7:520"
	], " "),
	?LOG_DEBUG("rrdcreate night command: ~p", [CmdNight]),
	os:cmd(CmdNight),
	file:change_owner(path(night, Sid), ?FLUKSO_UID, ?FLUKSO_GID),
	{ok, created};
create(_Sid, _Class, _Exists) ->
	{ok, not_created}.

path(base, Sid) ->
	[?RRD_PATH_BASE, binary_to_list(Sid), ".rrd"];
path(night, Sid) ->
	[?RRD_PATH_NIGHT, binary_to_list(Sid), ".rrd"].

join([Head | []], _Sep) ->
	[Head];
join([Head | Tail], Sep) ->
	[Head, Sep | join(Tail, Sep)].

