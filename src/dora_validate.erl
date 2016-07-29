-module(dora_validate).

-export([validate/2]).
-export([format_error/1]).

-type key() :: atom().
-type type() :: string | {integer, integer(), integer()} | ipv4_address | host | port_number | boolean | http_uri | list_http_uri | list_to_binary.
-type required() :: required | optional.


-spec validate(atom(), [{key(), type(), required()}]) -> ok | {error, term()}.
validate(_Applicaiton, []) ->
    ok;
validate(Application, [{Key, Type, required}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            {error, {required, Key}};
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end;
validate(Application, [{Key, Type, optional}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            validate(Application, Rest);
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end;
validate(Application, [{Key, Type, optional, Default}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            %% デフォルトの値は間違っていないという前提
            ok = application:set_env(Application, Key, Default),
            validate(Application, Rest);
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end.


validate0(Application, Rest, Key, Type, Value) ->
    case validate_type(Type, Value) of
        ok ->
            validate(Application, Rest);
        {ok, ConvertValue} ->
            %% 変換して戻ってくる可能性もある
            ok = application:set_env(Application, Key, ConvertValue),
            validate(Application, Rest);
        Reason when is_atom(Reason) ->
            {error, {Reason, Key, Type, Value}}
    end.


validate_type(string, Value) ->
    validate_string(Value);
validate_type({integer, Min, Max}, Value) ->
    validate_integer(Value, Min, Max);
validate_type(ipv4_address, Value) ->
    validate_ipv4_address(Value);
validate_type(host, Value) ->
    validate_host(Value);
validate_type(port_number, Value) ->
    validate_port_number(Value);
validate_type(boolean, Value) ->
    validate_boolean(Value);
validate_type(http_uri, Value) ->
    validate_http_uri(Value);
validate_type(list_http_uri, Value) ->
    validate_list_http_uri(Value);
validate_type(_UnknownType, _Value) ->
    unknown_type.


validate_port_number(Value) ->
    validate_integer(Value, 0, 65535).


validate_boolean(true) ->
    ok;
validate_boolean(false) ->
    ok;
validate_boolean(_) ->
    badarg.


validate_integer(Value, Min, infinity)
  when is_integer(Value) andalso Min =< Value ->
    ok;
validate_integer(Value, Min, Max)
  when is_integer(Value) andalso Min =< Value andalso Value =< Max ->
    ok;
validate_integer(_Value, _Min, _Max) ->
    badarg.


validate_ipv4_address(Value) ->
    case inet_parse:ipv4strict_address(Value) of
        {ok, IPAddress} ->
            {ok, IPAddress};
        {error, _Reason} ->
            badarg
    end.


validate_host(Value) ->
    case net_adm:dns_hostname(Value) of
        {ok, _Hostname} ->
            ok;
        {error, _Reason} ->
            badarg
    end.


%% FIXME(nakai): 手抜き
validate_string(Value) when is_list(Value) ->
    ok;
validate_string(_Value) ->
    badarg.


-include_lib("eunit/include/eunit.hrl").


validate_http_uri(Value) ->
    case http_uri:parse(Value) of
        {ok, _Result} ->
            ok;
        {error, _Reason} ->
            badarg
    end.


%% XXX(nakai): 最初から空だったのを許可するか？
    %% まずは空を許可する仕組みで作る
validate_list_http_uri([]) ->
    ok;
validate_list_http_uri([Value|Rest]) ->
    case validate_http_uri(Value) of
        ok ->
            validate_list_http_uri(Rest);
        badarg ->
            badarg
    end.



-spec format_error(term()) -> string(). 
format_error({unknown_type, Type, Value}) ->
    io_lib:format("CONFIG-BAD-VALUE | type=~s, value=~p", [Type, Value]);
format_error({badarg, Key, _Type, Value}) ->
    io_lib:format("CONFIG-BAD-VALUE | key=~s, value=~p", [Key, Value]);
format_error({required, Key}) ->
    io_lib:format("CONFIG-REQUIRED | key=~s", [Key]).
