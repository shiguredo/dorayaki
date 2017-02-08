-module(dora_datetime_tests).

-include_lib("eunit/include/eunit.hrl").

-include("dora_datetime.hrl").

-import(dora_datetime, [timestamp/2,
                        year/1, month/1, day/1,
                        hour/1, minute/1, second/1, micro_second/1, tz_offset/1,
                        iso8601/1, iso8601_no_micros/1, iso8601_no_millis/1, yyyymmdd/1,
                        iso8601_to_timestamp/1,
                        relativedelta/2,
                        posix_time_to_iso8601/1]).


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
     ?_assertEqual(<<"2013-11-11T23:45:58.000000Z">>, iso8601(timestamp({1384, 213558, 0}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-11T23:45:58.000001Z">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-12T08:45:58.000001+09:00">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_JST))),
     ?_assertEqual(<<"2013-11-11T18:45:58.000001-05:00">>, iso8601(timestamp({1384, 213558, 1}, ?DORA_TZ_ECT)))
    ].


iso8601_no_micros_test_() ->
    [
     ?_assertEqual(<<"2013-11-11T23:45:58.123Z">>, iso8601_no_micros(timestamp({1384, 213558, 123456}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-11T23:45:58.123Z">>, iso8601_no_micros(timestamp({1384, 213558, 123456}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-11T23:45:58.000Z">>, iso8601_no_micros(timestamp({1384, 213558, 1000000}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-12T08:45:58.123+09:00">>, iso8601_no_micros(timestamp({1384, 213558, 123456}, ?DORA_TZ_JST))),
     ?_assertEqual(<<"2013-11-11T18:45:58.123-05:00">>, iso8601_no_micros(timestamp({1384, 213558, 123456}, ?DORA_TZ_ECT)))
    ].

iso8601_no_millis_test_() ->
    [
     ?_assertEqual(<<"2013-11-11T23:45:58Z">>, iso8601_no_millis(timestamp({1384, 213558, 123456}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-11T23:45:58Z">>, iso8601_no_millis(timestamp({1384, 213558, 123456}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"2013-11-12T08:45:58+09:00">>, iso8601_no_millis(timestamp({1384, 213558, 123456}, ?DORA_TZ_JST))),
     ?_assertEqual(<<"2013-11-11T18:45:58-05:00">>, iso8601_no_millis(timestamp({1384, 213558, 123456}, ?DORA_TZ_ECT)))
    ].

iso8601_to_timestamp_test_() ->
    [
     ?_assertEqual({error, {unsupported_format, <<"-">>}}, iso8601_to_timestamp(<<"-">>)),
     ?_assertEqual({error, {unsupported_date_format, <<"201-11-11">>}}, iso8601_to_timestamp(<<"201-11-11T23:45:58.000000">>)),
     ?_assertEqual({error, {unsupported_time_format, <<"23:45:5.000000">>}}, iso8601_to_timestamp(<<"2013-11-11T23:45:5.000000">>)),

     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000000">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000001">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000000Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000001Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58.000001+09:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58.000001+0900">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58.000001-05:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58.000001-0500">>)),

     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.001">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.000Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58.001Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58.001+09:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58.001+0900">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58.001-05:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 1000}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58.001-0500">>)),

     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_UTC)}, iso8601_to_timestamp(<<"2013-11-11T23:45:58Z">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58+09:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_JST)}, iso8601_to_timestamp(<<"2013-11-12T08:45:58+0900">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58-05:00">>)),
     ?_assertEqual({ok, timestamp({1384, 213558, 0}, ?DORA_TZ_ECT)}, iso8601_to_timestamp(<<"2013-11-11T18:45:58-0500">>))
    ].

yyyymmdd_test_() ->
    [
     ?_assertEqual(<<"20131111">>, yyyymmdd(timestamp({1384, 213558, 1}, ?DORA_TZ_UTC))),
     ?_assertEqual(<<"20131112">>, yyyymmdd(timestamp({1384, 213558, 1}, ?DORA_TZ_JST)))
    ].



posix_time_to_iso8601_test_() ->
    [
     ?_assertEqual(<<"1998-12-31T23:59:59Z">>, posix_time_to_iso8601(915148799)),
     ?_assertEqual(<<"2038-01-19T03:14:07Z">>, posix_time_to_iso8601(2147483647))
    ].
