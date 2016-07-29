-module(dora_validate_tests).

-import(dora_validate, [validate/2]).

-include_lib("eunit/include/eunit.hrl").


validate_type_host_test() ->
    application:set_env(dorayaki, ip_address, "127.0.0.1"),
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, host, required}])),

    application:set_env(dorayaki, ip_address, "dorayaki"),
    ?assertEqual({error, {badarg, ip_address, host, "dorayaki"}},
                 validate(dorayaki, [{ip_address, host, required}])),

    ?assertEqual(ok,
                 application:unset_env(dorayaki, ip_address)),
    ok.


validate_type_ipv4_address_test() ->
    %% optional
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, ipv4_address, optional}])),
    ?assertEqual(undefined,
                 application:get_env(dorayaki, ip_address)),

    %% optional default
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, ipv4_address, optional, {127,0,0,1}}])),
    ?assertEqual({ok, {127,0,0,1}},
                 application:get_env(dorayaki, ip_address)),

    application:set_env(dorayaki, ip_address, "127.0.0.1"),
    ?assertEqual(ok,
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),

    application:set_env(dorayaki, ip_address, "abc"),
    ?assertEqual({error, {badarg, ip_address, ipv4_address, "abc"}},
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),

    application:set_env(dorayaki, ip_address, "::1"),
    ?assertEqual({error, {badarg, ip_address, ipv4_address, "::1"}},
                 validate(dorayaki, [{ip_address, ipv4_address, required}])),

    ?assertEqual(ok,
                 application:unset_env(dorayaki, ip_address)),
    ok.


validate_type_port_number_test() ->
    %% optional
    ?assertEqual(ok,
        validate(dorayaki, [{port_number, port_number, optional}])),
    ?assertEqual(undefined,
        application:get_env(dorayaki, port_number)),

    %% optional default
    ?assertEqual(ok,
        validate(dorayaki, [{port_number, port_number, optional, 1}])),
    ?assertEqual({ok, 1},
        application:get_env(dorayaki, port_number)),

    application:set_env(dorayaki, port_number, 1),
    ?assertEqual(ok,
        validate(dorayaki, [{port_number, port_number, required}])),

    application:set_env(dorayaki, port_number, 65535),
    ?assertEqual(ok,
        validate(dorayaki, [{port_number, port_number, required}])),

    %% bad range
    application:set_env(dorayaki, port_number, -1),
    ?assertEqual({error, {badarg, port_number, port_number, -1}},
        validate(dorayaki, [{port_number, port_number, required}])),

    application:set_env(dorayaki, port_number, 65536),
    ?assertEqual({error, {badarg, port_number, port_number, 65536}},
        validate(dorayaki, [{port_number, port_number, required}])),

    %% bad type
    application:set_env(dorayaki, port_number, "abc"),
    ?assertEqual({error, {badarg, port_number, port_number, "abc"}},
        validate(dorayaki, [{port_number, port_number, required}])),

    ?assertEqual(ok,
        application:unset_env(dorayaki, port_number)),
    ok.


validate_type_http_uri_test() ->
    %% optional
    ?assertEqual(ok,
        validate(dorayaki, [{webhook_url, http_uri, optional}])),
    ?assertEqual(undefined,
        application:get_env(dorayaki, webhook_url)),

    %% optional default
    ?assertEqual(ok,
        validate(dorayaki, [{webhook_url, http_uri, optional, []}])),
    ?assertEqual({ok, []},
        application:get_env(dorayaki, webhook_url)),

    %% https://
    application:set_env(dorayaki, webhook_url, "http://example.com"),
    ?assertEqual(ok,
        validate(dorayaki, [{webhook_url, http_uri, required}])),

    %% http://
    application:set_env(dorayaki, webhoo_url, "https//google.com"),
    ?assertEqual(ok,
        validate(dorayaki, [{webhook_url, http_uri, required}])),

    %% bad uri
    application:set_env(dorayaki, webhook_url, "ntp://example.com"),
    ?assertEqual({error, {badarg, webhook_url, http_uri, "ntp://example.com"}},
        validate(dorayaki, [{webhook_url, http_uri, required}])),

    %% bad type
    application:set_env(dorayaki, webhook_url, 1),
    ?assertEqual({error, {unknown_type, webhook_url, http_url, 1}},
                 %% http_ur*l*
        validate(dorayaki, [{webhook_url, http_url, required}])),

    ?assertEqual(ok,
        application:unset_env(dorayaki, webhook_url)),
    ok.


