-module(memcacherl_SUITE).

-export([all/0, groups/0,
	 init_per_group/2, end_per_group/2]).

-export([request_get/0, request_get/1]).
-export([response_get/0, response_get/1]).
-export([request_set/1]).

-include_lib("common_test/include/ct.hrl").
-include("src/memcacherl.hrl").

all() -> [
	  {group, request},
	  {group, response}
	 ].

init_per_group(request, Config) ->
    ok = application:start(memcacherl),
    Config;
init_per_group(response, Config) ->
    ok = application:start(memcacherl),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(request, Config) ->
    application:stop(memcacherl),
    Config;
end_per_group(response, Config) ->
    application:stop(memcacherl),
    Config;
end_per_group(_, Config) ->
    Config.

groups() ->
    [
     {request, [], [request_get,
		    request_set
		   ]},
     {response, [], [response_get]}
    ].

%% requests
request_get() ->
    [].
request_get(_Config) ->
    EncodedGet = memcacherl:encode(?REQUEST, ?GET, <<"Hello">>, <<"">>, []),
    EncodedGet = <<128,0,0,5,8,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,0,0,222,173,190,239,0,0,0,0,72,101,108,108,111>>,
    DecodedGet = memcacherl:decode(EncodedGet),
    DecodedGet = [{op, 0}, {data_type,0}, {status,0}, {opaque,0}, {cas,0}, {extras, <<222,173,190,239,0,0,0,0>>},
		  {key, <<"Hello">>}, {value, <<>>}].

request_set(_Config) ->
    EncodedSet = memcacherl:encode(?REQUEST, ?SET, <<"Hello">>, <<"World">>, [{expires, 20}]),
    EncodedSet = <<128,1,0,5,8,0,0,0,0,0,0,18,0,0,0,0,0,0,0,0,0,0,0,0,222,173,190,239,0,0,0,20,72,101,108,108,111,87,111,114,108,100>>,
    DecodedSet = memcacherl:decode(EncodedSet),
    ?SET = proplists:get_value(op, DecodedSet),
    ct:pal("~p~n", [DecodedSet]).

%% responses
response_get() ->
     [].
response_get(_Config) ->
    _EncodedGet = memcacherl:encode(?RESPONSE, ?GET, <<>>, <<"World">>, []).




