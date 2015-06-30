%%% @doc
%%% ==FLMQTT SQL==
%%% Prepared SQL statements used in the FLMQTT service.
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

-module(flmqtt_sql).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-export([prepare/1, execute/3]).

-define(MYSQL_FLUKSO, "/db/mysql/flukso").

-define(SQL_AUTH_DEVICE,
	"SELECT sha
	 FROM logger_devices
	 WHERE device = ?").
-define(SQL_AUTH_SENSOR,
	"SELECT device
	 FROM logger_meters
	 WHERE meter = ?").
-define(SQL_SENSORS,
	"SELECT meter
	 FROM logger_meters
	 WHERE device = ?").
-define(SQL_SENSORS_ACTIVE,
	"SELECT meter
	 FROM logger_meters
	 WHERE device = ? AND enabled = 1").
-define(SQL_SENSOR_CONFIG,
	"UPDATE logger_meters
	 SET
	 type = ?,
	 class = ?,
	 function = ?,
	 voltage = ?,
	 current = ?,
	 constant = ?,
	 kid = ?,
	 rid = ?,
	 data_type = ?,
	 enabled = ?,
	 port = ?,
	 config = ?
	 WHERE meter = ?").
-define(SQL_TMPO_SINK,
	"INSERT INTO tmpo (sensor, rid, lvl, bid, ext, created, data)
	 VALUES (?, ?, ?, ?, ?, ?, ?)").
-define(SQL_TMPO_CLEAN,
	"DELETE FROM tmpo
	 WHERE sensor = ? AND rid = ? AND lvl = ? AND bid <= ? AND ext = ?").
-define(SQL_TMPO_LAST,
	"SELECT sensor, rid, lvl, bid, ext
	 FROM tmpo
	 WHERE sensor = ?
	 ORDER BY created DESC, lvl DESC
	 LIMIT 1").

-define(STATEMENTS,
	[{auth_device, ?SQL_AUTH_DEVICE},
	 {auth_sensor, ?SQL_AUTH_SENSOR},
	 {sensors, ?SQL_SENSORS},
	 {sensors_active, ?SQL_SENSORS_ACTIVE},
	 {sensor_config, ?SQL_SENSOR_CONFIG},
	 {tmpo_sink, ?SQL_TMPO_SINK},
	 {tmpo_clean, ?SQL_TMPO_CLEAN},
	 {tmpo_last, ?SQL_TMPO_LAST}]).

prepare(Dispatcher) ->
	[cloudi_service_db_mysql:prepare_query(Dispatcher, ?MYSQL_FLUKSO, Id, Query)
		|| {Id, Query} <- ?STATEMENTS].

execute(Dispatcher, Id, Params) ->
	case cloudi_service_db_mysql:execute_query(Dispatcher, ?MYSQL_FLUKSO, Id, Params) of
		{ok, {ok, {mysql_result, _Structure, Result, _, _, _, _, [], []}}} ->
			?LOG_DEBUG("~p returns result ~p", [?MYSQL_FLUKSO, Result]),
			{ok, Result};
		{ok, {ok, {mysql_result, [], [], _, _, _, _, Update, []}}} ->
			?LOG_DEBUG("~p returns result ~p", [?MYSQL_FLUKSO, Update]),
			{ok, Update};
		{ok, {error, {mysql_result, [], [], _, _, _, _, [], Error}}} ->
			?LOG_ERROR("~p returns error ~p", [?MYSQL_FLUKSO, Error]),
			{error, Error};
		{ok, {error, "not prepared"}} ->
			?LOG_INFO("initializing prepared statements for ~p", [?MYSQL_FLUKSO]),
			prepare(Dispatcher),
			execute(Dispatcher, Id, Params)
	end. 

