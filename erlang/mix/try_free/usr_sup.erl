-module(usr_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({locall, ?MOULDE}, ?MOULDE, []).

init(FileName) ->
    UsrChild = {usr, {usr, start_link, []},
                permanent, 2000, worker, [usr, usr_db]},
    {ok, {{one_for_all, 1, 1}, [UsrChild]}}.
