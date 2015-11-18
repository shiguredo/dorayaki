-module(dora_datetime).

-export([timestamp/0, timestamp/1, timestamp/2]).
-export([year/1, month/1, day/1,
         hour/1, minute/1, second/1, micro_second/1, tz_offset/1]).
-export([timestamp_to_datetime/1, timestamp_to_gregorian_seconds/1,
         datetime_to_timestamp/1]).
-export([iso8601/1, iso8601_no_micros/1, iso8601_no_millis/1, yyyymmdd/1,
         iso8601_to_timestamp/1]).
-export([relativedelta/2]).

-export_type([month/0, day/0]).

-include("dora_datetime.hrl").

-type month() :: 1..12.
-type day() :: 1..31.

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
    Timestamp = erlang:timestamp(),
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


datetime_to_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    #dora_timestamp{year = Year,
                    month = Month,
                    day = Day,
                    hour = Hour,
                    minute = Minute,
                    second = Second}.


timestamp_to_gregorian_seconds(Timestamp) when is_record(Timestamp, dora_timestamp) ->
    Datetime = timestamp_to_datetime(Timestamp),
    calendar:datetime_to_gregorian_seconds(Datetime).


-spec iso8601(#dora_timestamp{}) -> binary().
iso8601(#dora_timestamp{year = Year, month = Month, day = Day,
                        hour = Hour, minute = Minute, second = Second, micro_second = MicroSecs,
                        tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b.~6.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, MicroSecs, TzDesignator])).


iso8601_no_micros(#dora_timestamp{
                     year = Year, month = Month, day = Day,
                     hour = Hour, minute = Minute, second = Second,
                     micro_second = MicroSecs,
                     tz_designator = TzDesignator
                    }) ->
    Milli = erlang:round(MicroSecs / 1000),
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, Milli, TzDesignator])).



iso8601_no_millis(#dora_timestamp{year = Year, month = Month, day = Day,
                                  hour = Hour, minute = Minute, second = Second,
                                  tz_designator = TzDesignator}) ->
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b~s",
                                 [Year, Month, Day, Hour, Minute, Second, TzDesignator])).


%% XXX(shimazaki): Cannot parse milliseconds now
-spec iso8601_to_timestamp(binary()) -> #dora_timestamp{}.
iso8601_to_timestamp(Binary) ->
    [Date, TimeAndTz] = binary:split(Binary, <<"T">>),
    DateRegExp = <<"^(\\d{4})\-*(\\d{2})\-*(\\d{2})$">>,
    {match, [YearBin, MonthBin, DayBin]} = re:run(Date, DateRegExp, [{capture, all_but_first, binary}]),

    TimeRegExp = <<"^(\\d{2})\:*(\\d{2})\:*(\\d{2})([\.\,]\\d{6})?(\.*)$">>,
    {HourBin, MinuteBin, SecondBin, MicroSecondBin, Tz} = case re:run(TimeAndTz, TimeRegExp, [{capture, all_but_first, binary}]) of
        {match, [HourBin0, MinuteBin0, SecondBin0, <<>>, Tz0]} ->
            {HourBin0, MinuteBin0, SecondBin0, <<"0">>, Tz0};
        {match, [HourBin0, MinuteBin0, SecondBin0, <<_:1/binary, MicroSecondBin0:6/binary>>, Tz0]} ->
            {HourBin0, MinuteBin0, SecondBin0, MicroSecondBin0, Tz0}
    end,

    Timestamp = #dora_timestamp{year = binary_to_integer(YearBin),
                                month = binary_to_integer(MonthBin),
                                day = binary_to_integer(DayBin),
                                hour = binary_to_integer(HourBin),
                                minute = binary_to_integer(MinuteBin),
                                second = binary_to_integer(SecondBin),
                                micro_second = binary_to_integer(MicroSecondBin)},
    case Tz of
        <<>> ->
            Timestamp;
        <<"Z">> ->
            Timestamp;
        <<"+", Hour:2/binary, ":00">> ->
            Timestamp#dora_timestamp{tz_offset = binary_to_integer(Hour),
                                     tz_designator = Tz};
        <<"-", Hour:2/binary, ":00">> ->
            Timestamp#dora_timestamp{tz_offset = -1 * binary_to_integer(Hour),
                                     tz_designator = Tz}
    end.


-spec yyyymmdd(calendar:date() | #dora_timestamp{}) -> binary().
yyyymmdd(#dora_timestamp{year = Year, month = Month, day = Day}) ->
    list_to_binary(io_lib:format("~4.10.0b~2.10.0b~2.10.0b", [Year, Month, Day])).
