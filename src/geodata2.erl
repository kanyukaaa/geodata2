-module(geodata2).

%% API
-export([lookup/1, start/0, stop/0]).
-include("geodata2.hrl").

-spec lookup(any()) -> {ok, Result::list()} | not_found | {error, Reason :: term()}.
lookup(IP) ->
    {data, Data} = ets:lookup(?GEODATA2_STATE_TID, data),
    {meta, Meta} = ets:lookup(?GEODATA2_STATE_TID, meta),
    case geodata2_ip:make_ip(IP) of
        {ok, Bits, IPV} ->
            geodata2_format:lookup(Meta, Data, Bits, IPV);
        {error, format} ->
            {error, format}
    end.

-spec start() -> ok | {error, Reason :: term()}.
start() ->
	application:start(geodata2).

stop() ->
	application:stop(geodata2).

%%%===================================================================
%%% Internal functions
%%%===================================================================
