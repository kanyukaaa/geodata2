-module(geodata2).
-behavior(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([lookup/1, start/0, start_link/1, stop/0]).
-include("geodata2.hrl").

-record(state, {}).

-spec lookup(any()) -> {ok, Result::list()} | not_found | {error, Reason :: term()}.
lookup(IP) ->
    [{data, Data}] = ets:lookup(?GEODATA2_STATE_TID, data),
    [{meta, Meta}] = ets:lookup(?GEODATA2_STATE_TID, meta),
    case geodata2_ip:make_ip(IP) of
        {ok, Bits, IPV} ->
            geodata2_format:lookup(Meta, Data, Bits, IPV);
        {error, format} ->
            {error, format}
    end.

start() ->
	application:start(geodata2).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop() ->
	application:stop(geodata2).

new(File) ->
    case filelib:is_file(File) of
        true ->
            {ok, Data} = file:read_file(File),
            {ok, Meta} = geodata2_format:meta(Data),
            ets:new(?GEODATA2_STATE_TID, [set, protected, named_table, {read_concurrency, true}]),
            ets:insert(?GEODATA2_STATE_TID, {data, Data}),
            ets:insert(?GEODATA2_STATE_TID, {meta, Meta}),
            {ok, #state{}};
        _ ->
            {stop, {geodata2_dbfile_not_found, File}}
    end.

init(_Args) ->
    case application:get_env(geodata2, dbfile) of
        {ok, File} ->
            new(File);
        _ ->
            {stop, {geodata2_dbfile_unspecified}}
    end.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
