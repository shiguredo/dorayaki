-module(dora_validate_tests).

-import(dora_validate, [validate/2]).

-include_lib("eunit/include/eunit.hrl").


validate_type_host_test() ->
    application:set_env(dorayaki, ip_address, "127.0.0.1"),
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, host, required}])),

    application:set_env(dorayaki, ip_address, "abc"),
    ?assertEqual({error, {badarg, ip_address, host, "abc"}},
                 validate(dorayaki, [{ip_address, host, required}])),
    ok.


validate_type_ipv4_address_test() ->
    application:set_env(dorayaki, ip_address, "127.0.0.1"),
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),

    application:set_env(dorayaki, ip_address, "abc"),
    ?assertEqual({error, {badarg, ip_address, ipv4_address, "abc"}},
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),

    application:set_env(dorayaki, ip_address, "::1"),
    ?assertEqual({error, {badarg, ip_address, ipv4_address, "::1"}},
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),
    ok.

