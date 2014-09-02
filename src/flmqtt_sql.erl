%%% @doc
%%% ==FLMQTT SQL==
%%% Prepared SQL statements used in the FLMQTT service.
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

-module(flmqtt_sql).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-export([prepare/1, execute/3]).

-define(MYSQL_FLUKSO, "/db/mysql/flukso").

-define(SQL_SENSORS,
	"SELECT meter FROM logger_meters WHERE device = ?").
-define(SQL_ENABLED_SENSORS,
	"SELECT meter FROM logger_meters WHERE device = ? AND enabled = 1").
-define(SQL_TMPO_SINK,
	"INSERT INTO tmpo (sensor, rid, lvl, bid, ext, created, data) VALUES (?, ?, ?, ?, ?, ?, ?)").
-define(SQL_TMPO_CLEAN,
	"DELETE FROM tmpo WHERE sensor = ? AND rid = ? AND lvl = ? AND bid <= ? AND ext = ?").

-define(STATEMENTS,
	[{sensors, ?SQL_SENSORS},
	 {enabled_sensors, ?SQL_ENABLED_SENSORS},
	 {tmpo_sink, ?SQL_TMPO_SINK},
	 {tmpo_clean, ?SQL_TMPO_CLEAN}]).

prepare(Dispatcher) ->
	[cloudi_service_db_mysql:prepare_query(Dispatcher, ?MYSQL_FLUKSO, Id, Query)
		|| {Id, Query} <- ?STATEMENTS].

execute(Dispatcher, Id, Params) ->
	case cloudi_service_db_mysql:execute_query(Dispatcher, ?MYSQL_FLUKSO, Id, Params) of
		{ok, {ok, {mysql_result, _Structure, Result, _, _, _, _, [], []}}} ->
			?LOG_DEBUG("~p returns result ~p", [?MYSQL_FLUKSO, Result]),
			{ok, Result};
		{ok, {error, {mysql_result, [], [], _, _, _, _, [], Error}}} ->
			?LOG_ERROR("~p returns error ~p", [?MYSQL_FLUKSO, Error]),
			{error, Error};
		{ok, {error, "not prepared"}} ->
			?LOG_WARN("initializing prepared statements for ~p", [?MYSQL_FLUKSO]),
			prepare(Dispatcher),
			execute(Dispatcher, Id, Params)
	end. 

