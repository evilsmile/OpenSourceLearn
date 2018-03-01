%%% File: usr.el
%%% Description: API and gen_server code for cellpone user db
-module(usr).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add_usr/3, delete_user/1, set_service/3, set_status/2, 
         delete_disable/0, lookup_id/1]).
-export([lookup_msisdn/1, service_flag/2]).
-behaviour(gen_server).

%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    start_link("usrDB").

start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
    gen_server:case(?MODULE, stop).

%% Customer Service API
add_usr(PhoneNum, CustId, Plan) when Plan == Prepay; Plan == postpay ->
    gen_server:call(?MODULE, {add_usr, PhoneNum, CustId, Plan}).

delete_usr(CustId) ->
    gen_server:call(?MODULE, {delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag == true; Flag == false ->
    gen_server:call(?MODULE, {set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status == enabled; Status == disabled ->
    gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disable() ->
    gen_server:call(?MODULE, delete_disable).

lookup_id(CustId) ->
    usr_db:lookup_id(CustId).

lookup_msisdn(PhoneNo) ->
    usr_db:lookup_msisdn(PhoneNo).

service_flag(PhoneNo, Service) ->
    case usr_db:lookup_msisdn(PhoneNo) of
        {ok, #usr{services=Services, status=enabled}} ->
            lists:member(Service, Services);
        {ok, #usr{status=disabled}} ->
            {error, disabled};
        {error, Reason} ->
            {error, Reason}
    end.

%% Callbackfunctions
init(FileName) ->
    usr_db:create_tables(FileName),
    usr_db:restore_backup(),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    usr_db:close_tables().

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({add_usr, PhoneNo, CustId, Plan}, _From, LoopData) ->
    Reply = usr_db:add_usr(#usr{msisdn = PhoneNo, 
                                id = CustId,
                                plan = Plan}),
    {reply, Reply, LoopData};

handle_call({delete_user, CustId}, _From, LoopData) ->
    Reply = usr_db:delete_usr(CustId),
    {reply, Reply, LoopData};
handle_call({set_service, CustId, Service, Flag}, _From, LoopData) ->
    Reply = case usr_db:lookup_id(CustId) of
                {ok, Usr} ->
                    Services = lists:delete(Service, Usr#usr.services),
                    NewServices = case Flag of
                                      true -> [Service|Services];
                                      false -> Services
                                  end,
                    usr_db:update_usr(Usr#usr{services=NewServices});
                {error, instance} ->
                    {error, instance}
            end,
    {reply, Reply, LoopData};
handle_call({set_status, CustId, Status}, _From, LoopData) ->
    Reply = case usr_db:lookup_id(CustId) of
                {ok, Usr} ->
                    usr_db:update_usr(Usr#usr{status=Status});
                {error, instance} ->
                    {error, instance}
            end,
    {reply, Reply,LoopData};

handle_call(delete_disable, _From, LoopData) ->
    {reply, usr_db:delete_disable(), LoopData}.
