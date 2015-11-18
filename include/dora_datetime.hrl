-define(DORA_TZ_UTC, utc).
-define(DORA_TZ_JST, jst).
-define(DORA_TZ_ECT, ect).


-record(dora_timestamp, {
          year :: non_neg_integer(),
          month :: dora_datetime:month(),
          day :: dora_datetime:day(),
          hour :: 0..23,
          minute :: 0..59,
          second :: 0..59,
          micro_second :: 0..999999,
          tz_offset = 0 :: integer(),
          tz_designator = <<"Z">> :: binary()
         }).
