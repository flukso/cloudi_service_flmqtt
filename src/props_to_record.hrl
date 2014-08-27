%%% @doc
%%% ==props_to_record.hrl==
%%% Macro converting a proplist into a record.
%%% ?PROPS_TO_RECORD(PropList, RecordSymbol) -> Record 
%%% @end
%%%
%%% MIT LICENSE
%%%
%%% Copyright (c) 2012 Sungjin Park <jinni.park@gmail.com>
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
%%% @author Sungjin Park <jinni.park@gmail.com>
%%% @copyright 2012 Sungjin Park
%%% @version 0.1.0 {@date} {@time}

-author("Sungjin Park <jinni.park@gmail.com>").

%% @doc Convert a proplist into a record.
%% @spec ?PROPS_TO_RECORD(Props=[{atom(), term()}], Record=atom()) -> record().
-define(PROPS_TO_RECORD(Props, Record), ?PROPS_TO_RECORD(Props, Record, #Record{})()).
-define(PROPS_TO_RECORD(Props, Record, Default),
		fun() ->
			Fields = record_info(fields, Record),
			[Record | Defaults] = tuple_to_list(Default),
			List = [proplists:get_value(F, Props, D) || {F, D} <- lists:zip(Fields, Defaults)],
			list_to_tuple([Record | List])
		end).

-define(RECORD_TO_PROPS(Record, RecordDef), ?RECORD_TO_PROPS(Record, RecordDef, null)()).
-define(RECORD_TO_PROPS(Record, RecordDef, _Dummy),
		fun() ->
				Keys = record_info(fields, RecordDef),
				[RecordDef | Values] = tuple_to_list(Record),
				lists:zip(Keys, Values)
		end).
