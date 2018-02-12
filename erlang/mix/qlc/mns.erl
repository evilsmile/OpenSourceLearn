-module(mns).
-export([init/0, select/0, insert/0, test/0]).
-include_lib("stdlib/include/qlc.hrl").

%% 账号表结构
-record(y_account, {id, account, password}).

%% 资料表结构
-record(y_info, {id, nickname, birthday, sex}).

init() ->
    mnesia:create_schema([node()]),

    mnesia:start(),
    
    %% create table y_account (id int, account varchar(50), password varchar(50), primary key(id));
    mnesia:create_table(y_account, [{attributes, record_info(fields, y_account)}, {type, set}, {disc_copies, [node()]} ]).

query2(Q) ->
    F = fun() ->
        MatchHead = #y_account{id = '$1', account='$2', _='_'},
        Guard = [],
        Result = ['$$'],
        mnesia:select(y_account, [{MatchHead, Guard, Result}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

query(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

select() ->
    %%query(qlc:q([X || X <- mnesia:table(y_account)])).
    query2(qlc:q([X || X <- mnesia:table(y_account)])).

insert() ->
    Accounts = [#y_account{id=22, account="JY", password="love"}],
    insert_record(Accounts).

insert_record([]) -> ok;
insert_record([H|T]) ->
    do_insert(H),
    insert_record(T).

do_insert(Record) ->
    mnesia:transaction(fun() -> mnesia:write(Record) end).

delete() ->
    ok.

test() ->
    init(),
    insert(),
    select().
    %%delete().
