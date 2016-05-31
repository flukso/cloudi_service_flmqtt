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

-export([create/3, update/4]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(FLM03E_HARDWARE, 35).
-define(FLUKSO_UID, 1000).
-define(FLUKSO_GID, 1000).
-define(FLUKSO_PATH_DATA , "/var/lib/flukso/data/").
-define(RRD_PATH_BASE, [?FLUKSO_PATH_DATA, "base/"]).
-define(RRD_PATH_NIGHT, [?FLUKSO_PATH_DATA, "night/"]).
-define(RRD_CREATE_TYPES, [
	{<<"electricity">>, true},
	{<<"gas">>, true},
	{<<"water">>, true}
]).
-define(RRD_CREATE_SUBTYPES, [
	{<<"pplus">>, true},
	{undefined, true}
]).

create(Sid, Type, Subtype) ->
	create(Sid, proplists:get_value(Type, ?RRD_CREATE_TYPES),
		proplists:get_value(Subtype, ?RRD_CREATE_SUBTYPES),
		file:read_file_info(path(base, Sid))).

create(Sid, true, true, {error, enoent}) ->
	CmdBase = erlrrd:c([
		path(base, Sid),
		"-b 1199487600",
		"-s 60",
		"DS:meter:DERIVE:8640000:-20:20",
		"RRA:AVERAGE:0.5:1:1440",
		"RRA:AVERAGE:0.5:15:672",
		"RRA:AVERAGE:0.5:1440:365",
		"RRA:AVERAGE:0.5:10080:520"
	]),
	?LOG_DEBUG("rrdcreate base command: ~p", [CmdBase]),
	erlrrd:create(CmdBase),
	file:change_owner(path(base, Sid), ?FLUKSO_UID, ?FLUKSO_GID),
	CmdNight = erlrrd:c([
		path(night, Sid),
		"-b 1199487600",
		"-s 86400",
		"DS:meter:GAUGE:8640000:-20:20",
		"RRA:AVERAGE:0.5:1:365",
		"RRA:AVERAGE:0.5:7:520"
	]),
	?LOG_DEBUG("rrdcreate night command: ~p", [CmdNight]),
	erlrrd:create(CmdNight),
	file:change_owner(path(night, Sid), ?FLUKSO_UID, ?FLUKSO_GID),
	{ok, created};
create(_Sid, _Type, _Subtype, _Exists) ->
	{ok, not_created}.

% only update existing rrd from a tmpo block8 for FLM03E+
update(Hardware, Sid, 8, Gzdata) when Hardware >= ?FLM03E_HARDWARE ->
	update(Sid, file:read_file_info(path(base, Sid)), Gzdata);
update(_Hardware, _Sid, _Lvl, _Gzdata) ->
	{ok, no_rrd_update}.

update(Sid, {ok, _Fileinfo}, Gzdata) ->
	Jdata = cloudi_x_jsx:decode(zlib:gunzip(Gzdata), [{labels, atom}]),
	?LOG_DEBUG("rrd:update Jdata: ~p", [Jdata]),
	Trel = proplists:get_value(t, Jdata),
	Vrel = proplists:get_value(v, Jdata),
	[Thead, Vhead] = proplists:get_value(head,
		proplists:get_value(h, Jdata)),
	Tabs = rel_to_abs(tl(Trel), [Thead]),
	Vabs = rel_to_abs(tl(Vrel), [Vhead]),
	Data = [[integer_to_list(Time), ":", integer_to_list(Value), " "]
		|| {Time, Value} <- purge(lists:zip(Tabs, Vabs))],
	Return = erlrrd:update([path(base, Sid), " ", Data]),
	?LOG_DEBUG("rrd:update Data: ~p Return: ~p", [Data, Return]);
update(_Sid, {error, enoent}, _Gzdata) ->
	{ok, no_rrd_found}.

rel_to_abs([], Abs) ->
	lists:reverse(Abs);
rel_to_abs([Hrel | Trel], [Habs | Tabs]) ->
	rel_to_abs(Trel, [Hrel + Habs | [Habs | Tabs]]).

purge([]) ->
	[];
purge([{_Time, Value} | T]) ->
	purge(T, [], trunc(Value)).
 
purge([], Acc, _RefValue) ->
	lists:reverse(Acc);
purge([{Time, Value} | T], Acc, RefValue) when trunc(Value) =/= RefValue ->
	purge(T, [{Time, trunc(Value)} | Acc], trunc(Value));
purge([_H | T], Acc, RefValue) ->
	purge(T, Acc, RefValue).

path(base, Sid) ->
	[?RRD_PATH_BASE, binary_to_list(Sid), ".rrd"];
path(night, Sid) ->
	[?RRD_PATH_NIGHT, binary_to_list(Sid), ".rrd"].

