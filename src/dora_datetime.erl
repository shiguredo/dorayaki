-module(dora_datetime).

-export([timestamp/0, timestamp/1, timestamp/2]).
-export([year/1, month/1, day/1,
         hour/1, minute/1, second/1, micro_second/1, tz_offset/1]).
-export([iso8601/1, yyyymmdd/1]).

-include("dora_datetime.hrl").

-type month() :: 1..12.
-type day() :: 1..31.

-record(dora_datetime, {year :: non_neg_integer(),
                        month :: month(),
                        day :: day(),
                        hour :: 0..23,
                        minute :: 0..59,
                        second :: 0..59,
                        micro_second :: 0..999999,
                        tz_offset = 0 :: non_neg_integer(),
                        tz_designator = <<"Z">> :: binary()}).


-spec year(#dora_datetime{}) -> non_neg_integer().
year(#dora_datetime{year = Year}) ->
    Year.


-spec month(#dora_datetime{}) -> month().
month(#dora_datetime{month = Month}) ->
    Month.


-spec day(#dora_datetime{}) -> day().
day(#dora_datetime{day = Day}) ->
    Day.


-spec hour(#dora_datetime{}) -> 0..23.
hour(#dora_datetime{hour = Hour}) ->
    Hour.


-spec minute(#dora_datetime{}) -> 0..59.
minute(#dora_datetime{minute = Minute}) ->
    Minute.


second(#dora_datetime{second = Second}) ->
    Second.


micro_second(#dora_datetime{micro_second = MicroSecond}) ->
    MicroSecond.


-spec tz_offset(#dora_datetime{}) -> non_neg_integer().
tz_offset(#dora_datetime{tz_offset = TzOffset}) ->
    TzOffset.


-spec time_zone(atom()) -> binary().
time_zone(?DORA_TZ_UTC) ->
    {0, <<"Z">>};
time_zone(?DORA_TZ_JST) ->
    {9, <<"+09:00">>};
time_zone(?DORA_TZ_ECT) ->
    {-5, <<"-05:00">>};
time_zone(Tz) ->
    error({not_implemented, Tz}).


-spec timestamp() -> #dora_datetime{}.
timestamp() ->
    timestamp(?DORA_TZ_UTC).

-spec timestamp(atom()) -> #dora_datetime{}.
timestamp(Tz) ->
    Timestamp = os:timestamp(),
    timestamp(Timestamp, Tz).

-spec timestamp(erlang:timestamp(), atom()) -> #dora_datetime{}.
timestamp({_MegaSecs, _Secs, MicroSecs} = Timestamp, Tz) ->
    Datetime = calendar:now_to_universal_time(Timestamp),
    {Offset, Designator} = time_zone(Tz),
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime) + (60 * 60 * Offset),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Seconds),
    #dora_datetime{year = Year,
                   month = Month,
                   day = Day,
                   hour = Hour,
                   minute = Minute,
                   second = Second,
                   micro_second = MicroSecs,
                   tz_offset = Offset,
                   tz_designator = Designator}.


-spec iso8601(#dora_datetime{}) -> binary().
iso8601(#dora_datetime{year = Year, month = Month, day = Day,
                         hour = Hour, minute = Minute, second = Second, micro_second = 0,
                         tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, TzDesignator]));
iso8601(#dora_datetime{year = Year, month = Month, day = Day,
                         hour = Hour, minute = Minute, second = Second, micro_second = MicroSecs,
                         tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b.~6.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, MicroSecs, TzDesignator])).


-spec yyyymmdd(calendar:date() | #dora_datetime{}) -> binary().
yyyymmdd(#dora_datetime{year = Year, month = Month, day = Day}) ->
    list_to_binary(io_lib:format("~4.10.0b~2.10.0b~2.10.0b", [Year, Month, Day])).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


datetime_test() ->
    Datetime = timestamp({1384, 213558, 1}, ?DORA_TZ_UTC),
    ?assertEqual(2013, year(Datetime)),
    ?assertEqual(11, month(Datetime)),
    ?assertEqual(11, day(Datetime)),
    ?assertEqual(23, hour(Datetime)),
    ?assertEqual(45, minute(Datetime)),
    ?assertEqual(58, second(Datetime)),
    ?assertEqual(1, micro_second(Datetime)),
    ?assertEqual(0, tz_offset(Datetime)),
    ok.


iso8601_test_() ->
    [
     ?_assertEqual(<<"2013-11-11T23:45:58Z">>, iso8601(timestamp({1384, 213558, 0}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-11T23:45:58.000001Z">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-12T08:45:58.000001+09:00">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_JST))),
     ?_assertEqual(<<"2013-11-11T18:45:58.000001-05:00">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_ECT)))
    ].


yyyymmdd_test_() ->
    [
     ?_assertEqual(<<"20131111">>, yyyymmdd(timestamp({1384, 213558, 1}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"20131112">>, yyyymmdd(timestamp({1384, 213558, 1}, ?DORA_TZ_JST)))
    ].

-endif.

