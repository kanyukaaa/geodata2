-module(geodata2_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-include("geodata2.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case application:get_env(geodata2, dbfile) of
        {ok, File} ->
            {ok, Data} = file:read_file(File),
            {ok, Meta} = geodata2_format:meta(Data),
            ets:new(?GEODATA2_STATE_TID, [set, protected, named_table, {read_concurrency, true}]),
            ets:insert(?GEODATA2_STATE_TID, {data, Data}),
            ets:insert(?GEODATA2_STATE_TID, {meta, Meta}),

            RestartStrategy = one_for_one,
            MaxRestarts = 5,
            MaxSecondsBetweenRestarts = 60,
            SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
            {ok, {SupFlags}};
        _ ->
            {error, {geodata2_dbfile_unspecified}}
    end.

