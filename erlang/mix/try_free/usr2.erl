-module(usr2).
-export([init/0, create_table/0, ensure_loaded/0, insert/0]).
%-export([add_usr/3, delete_usr/1, set_service/3, set_status/2,
%         delete_disabled/0, lookup_id/1]).
%-export([lookup_msisdn/1, service_flag/2]).

-record(user, {id, name, age}).

init() ->
    {atomic, ok} = mnesia:create_schema([node()]),
    mnesia:start().

create_table() ->
    {atomic, ok} = mnesia:create_table(user, [{disc_copies, [node()]}, {ram_copies, nodes()},
                              {type, set}, {attributes, record_info(fields, user)},
                              {index, [id]}]).
ensure_loaded() ->
    ok = mnesia:wait_for_tables([user], 60000).

insert() ->
    Rec = #user{name=freely, id=2, age=33},
    mnesia:transaction(fun() -> mnesia:write(Rec) end).
