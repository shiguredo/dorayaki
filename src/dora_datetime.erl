-module(dora_datetime).

-export([timestamp/0, timestamp/1, timestamp/2]).
-export([year/1, month/1, day/1,
         hour/1, minute/1, second/1, micro_second/1, tz_offset/1]).
-export([relativedelta/2]).
-export([iso8601/1, yyyymmdd/1]).

-include("dora_datetime.hrl").

-type month() :: 1..12.
-type day() :: 1..31.

-record(dora_timestamp, {year :: non_neg_integer(),
                         month :: month(),
                         day :: day(),
                         hour :: 0..23,
                         minute :: 0..59,
                         second :: 0..59,
                         micro_second :: 0..999999,
                         tz_offset = 0 :: non_neg_integer(),
                         tz_designator = <<"Z">> :: binary()}).


-spec year(#dora_timestamp{}) -> non_neg_integer().
year(#dora_timestamp{year = Year}) ->
    Year.


-spec month(#dora_timestamp{}) -> month().
month(#dora_timestamp{month = Month}) ->
    Month.


-spec day(#dora_timestamp{}) -> day().
day(#dora_timestamp{day = Day}) ->
    Day.


-spec hour(#dora_timestamp{}) -> 0..23.
hour(#dora_timestamp{hour = Hour}) ->
    Hour.


-spec minute(#dora_timestamp{}) -> 0..59.
minute(#dora_timestamp{minute = Minute}) ->
    Minute.


second(#dora_timestamp{second = Second}) ->
    Second.


micro_second(#dora_timestamp{micro_second = MicroSecond}) ->
    MicroSecond.


-spec tz_offset(#dora_timestamp{}) -> non_neg_integer().
tz_offset(#dora_timestamp{tz_offset = TzOffset}) ->
    TzOffset.


-spec relativedelta(#dora_timestamp{}, {atom(), integer()}) -> #dora_timestamp{}.
relativedelta(_Timestamp, {years, _Value}) ->
    error(not_implemented);
relativedelta(_Timestamp, {months, _Value}) ->
    error(not_implemented);
relativedelta(Timestamp, {days, Value}) ->
    GregorianSeconds = timestamp_to_gregorian_seconds(Timestamp),
    GregorianSeconds1 = GregorianSeconds + (24 * 60 * 60) * Value,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(GregorianSeconds1),
    Timestamp#dora_timestamp{year = Year, month = Month, day = Day,
                             hour = Hour, minute = Minute, second = Second};
relativedelta(Timestamp, {hours, Value}) ->
    GregorianSeconds = timestamp_to_gregorian_seconds(Timestamp),
    GregorianSeconds1 = GregorianSeconds + (60 * 60) * Value,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(GregorianSeconds1),
    Timestamp#dora_timestamp{year = Year, month = Month, day = Day,
                             hour = Hour, minute = Minute, second = Second};
relativedelta(_Timestamp, {minites, _Value}) ->
    error(not_implemented);
relativedelta(_Timestamp, {seconds, _Value}) ->
    error(not_implemented);
relativedelta(_Timestamp, {microseconds, _Value}) ->
    error(not_implemented).




-spec time_zone(atom()) -> binary().
time_zone(?DORA_TZ_UTC) ->
    {0, <<"Z">>};
time_zone(?DORA_TZ_JST) ->
    {9, <<"+09:00">>};
time_zone(?DORA_TZ_ECT) ->
    {-5, <<"-05:00">>};
time_zone(Tz) ->
    error({not_implemented, Tz}).


-spec timestamp() -> #dora_timestamp{}.
timestamp() ->
    timestamp(?DORA_TZ_UTC).

-spec timestamp(atom()) -> #dora_timestamp{}.
timestamp(Tz) ->
    Timestamp = os:timestamp(),
    timestamp(Timestamp, Tz).

-spec timestamp(erlang:timestamp(), atom()) -> #dora_timestamp{}.
timestamp({_MegaSecs, _Secs, MicroSecs} = Timestamp, Tz) ->
    Datetime = calendar:now_to_universal_time(Timestamp),
    {Offset, Designator} = time_zone(Tz),
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime) + (60 * 60 * Offset),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Seconds),
    #dora_timestamp{year = Year,
                    month = Month,
                    day = Day,
                    hour = Hour,
                    minute = Minute,
                    second = Second,
                    micro_second = MicroSecs,
                    tz_offset = Offset,
                    tz_designator = Designator}.


timestamp_to_datetime(#dora_timestamp{year = Year,
                                      month = Month,
                                      day = Day,
                                      hour = Hour,
                                      minute = Minute,
                                      second = Second}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}}.


timestamp_to_gregorian_seconds(Timestamp) when is_record(Timestamp, dora_timestamp) ->
    Datetime = timestamp_to_datetime(Timestamp),
    calendar:datetime_to_gregorian_seconds(Datetime).


-spec iso8601(#dora_timestamp{}) -> binary().
iso8601(#dora_timestamp{year = Year, month = Month, day = Day,
                        hour = Hour, minute = Minute, second = Second, micro_second = 0,
                        tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, TzDesignator]));
iso8601(#dora_timestamp{year = Year, month = Month, day = Day,
                        hour = Hour, minute = Minute, second = Second, micro_second = MicroSecs,
                        tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b.~6.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, MicroSecs, TzDesignator])).


-spec yyyymmdd(calendar:date() | #dora_timestamp{}) -> binary().
yyyymmdd(#dora_timestamp{year = Year, month = Month, day = Day}) ->
    list_to_binary(io_lib:format("~4.10.0b~2.10.0b~2.10.0b", [Year, Month, Day])).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


timestamp_test() ->
    Timestamp = timestamp({1384, 213558, 1}, ?DORA_TZ_UTC),
    ?assertEqual(2013, year(Timestamp)),
    ?assertEqual(11, month(Timestamp)),
    ?assertEqual(11, day(Timestamp)),
    ?assertEqual(23, hour(Timestamp)),
    ?assertEqual(45, minute(Timestamp)),
    ?assertEqual(58, second(Timestamp)),
    ?assertEqual(1, micro_second(Timestamp)),
    ?assertEqual(0, tz_offset(Timestamp)),
    ok.


relativedelta_test() ->
    Timestamp = timestamp({1384, 213558, 1}, ?DORA_TZ_UTC),
    ?assertEqual(#dora_timestamp{year = 2013,
                                 month = 12,
                                 day = 1,
                                 hour = 23,
                                 minute = 45,
                                 second = 58,
                                 micro_second = 1,
                                 tz_offset = 0},
                 relativedelta(Timestamp, {days, 20})),
    ?assertEqual(#dora_timestamp{year = 2013,
                                 month = 11,
                                 day = 10,
                                 hour = 23,
                                 minute = 45,
                                 second = 58,
                                 micro_second = 1,
                                 tz_offset = 0},
                 relativedelta(Timestamp, {days, -1})),
    ?assertEqual(#dora_timestamp{year = 2013,
                                 month = 11,
                                 day = 11,
                                 hour = 22,
                                 minute = 45,
                                 second = 58,
                                 micro_second = 1,
                                 tz_offset = 0},
                 relativedelta(Timestamp, {hours, -1})),
    ?assertEqual(#dora_timestamp{year = 2013,
                                 month = 11,
                                 day = 12,
                                 hour = 0,
                                 minute = 45,
                                 second = 58,
                                 micro_second = 1,
                                 tz_offset = 0},
                 relativedelta(Timestamp, {hours, 1})),
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

