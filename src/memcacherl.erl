-module(memcacherl).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([start/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_request_opt/2]).

-export([encode/5]).
-export([decode/1]).

-include("memcacherl.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    ensure_started(memcacherl).

stop() ->
    gen_server:call(?MODULE, terminate).

encode(Type, Op, Key, Value, Options) when is_binary(Key), is_binary(Value) ->
    gen_server:call(?MODULE, {encode, Type, Op, Key, Value, Options}).

decode(Message) when is_binary(Message) ->
    gen_server:call(?MODULE, {decode, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({encode, Type, Op, Key, Value, Options}, _From, State) ->
    Message1 = get_request_options(Options),
    Message2 = Message1#message{magic=Type, op=Op, key=Key, value=Value},
    {reply, encode(Message2), State};
handle_call({decode, <<_Magic:8, Op:8, _KeyLength:16, _ExtrasLength:8, DataType:8, Status:16,
		       _TotalBodyLength:32, Opaque:32, CAS:64, _Body/binary>>}, _From, State) ->
    <<Extras:_ExtrasLength/binary, Key:_KeyLength/binary, Value/binary>> = _Body,
    Message = [{op, Op}, {data_type, DataType}, {status, Status}, {opaque, Opaque}, {cas, CAS},
	       {extras, Extras}, {key, Key}, {value, Value}],
    {reply, Message, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%
get_request_options(Options) ->
    Opts = #message{},
    get_request_opt(Options, Opts).

get_request_opt([], Message) ->
    Message;
get_request_opt([{expires, E} | Rest], Message) ->
    get_request_opt(Rest, Message#message{expires = E});
get_request_opt([{flags, F} | Rest], Message) ->
    get_request_opt(Rest, Message#message{flags = F});
get_request_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

encode(Message) when is_record(Message, message) ->
    Magic = Message#message.magic,
    Op = Message#message.op,
    KeyLength = size(Message#message.key),
    DataType = Message#message.data_type,
    ReservedStatus = Message#message.reserved_status,
    Opaque = Message#message.opaque,
    CAS = Message#message.cas,
    Extras = <<(Message#message.flags):32,
	       (Message#message.expires):32>>,
    ExtrasLength = size(Extras),
    Body = <<Extras/binary,
	     (Message#message.key)/binary,
	     (Message#message.value)/binary>>,
    TotalBodyLength = size(Body),
    <<Magic:8, Op:8, KeyLength:16, ExtrasLength:8, DataType:8, ReservedStatus:16, TotalBodyLength:32,
      Opaque:32, CAS:64, Body/binary>>.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

