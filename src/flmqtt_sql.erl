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
	<<"SELECT sha, sensor
	 FROM logger_devices
	 WHERE device = ?">>).
-define(SQL_AUTH_SENSOR,
	<<"SELECT device
	 FROM logger_meters
	 WHERE meter = ?">>).
-define(SQL_SENSORS,
	<<"SELECT meter
	 FROM logger_meters
	 WHERE device = ?">>).
-define(SQL_SENSORS_ACTIVE,
	<<"SELECT meter
	 FROM logger_meters
	 WHERE device = ? AND enabled = 1">>).
-define(SQL_SENSOR_CONFIG,
	<<"UPDATE logger_meters
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
	 ports = ?,
	 config = ?
	 WHERE meter = ?">>).
-define(SQL_SENSOR_CONFIG3,
	<<"UPDATE logger_meters
	 SET
	 type = ?,
	 subtype = ?,
	 class = ?,
	 kid = ?,
	 rid = ?,
	 data_type = ?,
	 enabled = ?,
	 ports = ?,
	 config = ?
	 WHERE meter = ?">>).
-define(SQL_KUBES_CLEAR,
	<<"UPDATE kube
	 SET
	 name = NULL,
	 config = ?,
	 kid = NULL,
	 enabled = 0
	 WHERE device = ? AND kid IS NOT NULL">>).
-define(SQL_KUBE_UPDATE,
	<<"UPDATE kube
	 SET
	 name = ?,
	 hw_type = ?,
	 sw_version = ?,
	 enabled = ?,
	 device = ?,
	 kid = ?,
	 config = ?
	 WHERE hw_id = ?">>).
-define(SQL_KUBE_INSERT,
	<<"INSERT INTO kube (
	 name,
	 hw_type,
	 sw_version,
	 enabled,
	 device,
	 kid,
	 created,
	 hw_id,
	 config)
	 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)">>).
-define(SQL_PORT_UPDATE,
	<<"UPDATE port
	 SET
	 name = ?,
	 class = ?,
	 current = ?,
	 triggr = ?,
	 constant = ?,
	 dsmr = ?,
	 enabled = ?,
	 config = ?
	 WHERE device = ? AND port = ?">>).
-define(SQL_PORT_INSERT,
	<<"INSERT INTO port (
	 name,
	 class,
	 current,
	 triggr,
	 constant,
	 dsmr,
	 enabled,
	 created,
	 device,
	 port,
	 config)
	 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>).
-define(SQL_SENSOR_COMPAT,
	<<"UPDATE logger_meters
	 SET
	 function = ?
	 WHERE device = ? AND ports = ? AND
	 ((type = 'electricity' AND subtype = 'pplus') OR type = 'water' OR type = 'gas')">>).
-define(SQL_DEVICE_SINK_TAP,
	<<"UPDATE logger_devices
	 SET
	 tap = ?
	 WHERE device = ?">>).
-define(SQL_TMPO_SINK,
	<<"INSERT INTO tmpo (sensor, rid, lvl, bid, ext, created, data)
	 VALUES (?, ?, ?, ?, ?, ?, ?)">>).
-define(SQL_TMPO_CLEAN,
	<<"DELETE FROM tmpo
	 WHERE sensor = ? AND rid = ? AND lvl = ? AND bid <= ? AND ext = ?">>).
-define(SQL_TMPO_LAST,
	<<"SELECT sensor, rid, lvl, bid, ext
	 FROM tmpo
	 WHERE sensor = ?
	 ORDER BY created DESC, lvl DESC
	 LIMIT 1">>).

-define(STATEMENTS,
	[{auth_device, ?SQL_AUTH_DEVICE},
	 {auth_sensor, ?SQL_AUTH_SENSOR},
	 {sensors, ?SQL_SENSORS},
	 {sensors_active, ?SQL_SENSORS_ACTIVE},
	 {sensor_config, ?SQL_SENSOR_CONFIG},
	 {sensor_config3, ?SQL_SENSOR_CONFIG3},
	 {kubes_clear, ?SQL_KUBES_CLEAR},
	 {kube_update, ?SQL_KUBE_UPDATE},
	 {kube_insert, ?SQL_KUBE_INSERT},
	 {port_update, ?SQL_PORT_UPDATE},
	 {port_insert, ?SQL_PORT_INSERT},
	 {sensor_compat, ?SQL_SENSOR_COMPAT},
	 {device_sink_tap, ?SQL_DEVICE_SINK_TAP},
	 {tmpo_sink, ?SQL_TMPO_SINK},
	 {tmpo_clean, ?SQL_TMPO_CLEAN},
	 {tmpo_last, ?SQL_TMPO_LAST}]).

prepare(Dispatcher) ->
	Result = [cloudi_service_db_mysql:prepare_query(Dispatcher, ?MYSQL_FLUKSO, Id, Query)
		|| {Id, Query} <- ?STATEMENTS],
	?LOG_DEBUG("~p prepared statement result: ~p", [?MYSQL_FLUKSO, Result]).

execute(Dispatcher, Id, Params) ->
	{{ok, Result}, _Pid} = cloudi_service_db_mysql:execute_query(Dispatcher, ?MYSQL_FLUKSO, Id, Params),
	?LOG_DEBUG("~p ~p query with params ~p result: ~p", [?MYSQL_FLUKSO, Id, Params, Result]),
    Result.
