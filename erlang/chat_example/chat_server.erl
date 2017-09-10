%%% Message passing utility.
%%% User Interface:
%%% logon(Name)
%%%   One user at a time can log in from each Erlang node in the system 
%%%   messenger: and choose a suitable Name. If the Name is already logged 
%%%   in at another node or if someone else if already logged in at the
%%%   same node, login will be rejected with a suitable error msg.
%%% off()
%%%   Logs off anybody at that node
%%% message(ToName, Message)
%%%   sends Message to ToName. Error message if the user of this function
%%%   is not logged on or if ToName is not logged on at any node.
%%% 
%%% One node in the network of Erlang nodes runs a server which maintains data about the logged on users. The server is registered as 'messenger'.
%%% Each node where there is a user logged on runs a client process registered as "mess_client".
%%%
%%% Protocol between the client processes and the server
%%% ---------------------------------------------------------
%%% To server: {ClientPid, logon, UserName}
%%% Reply {messenger, stop, user_exists_at_other_node } stops the client
%%% Reply {messenger, logged_on} logon was successful
%%%
%%% To server: {ClientPid, logoff}
%%% Reply: {messenger, logged_off}
%%%
%%% To server: {ClientPid, message_to, ToName, Message} send a message
%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%% Reply: {messenger, receiver_not_found} no user with this name
%%% Reply: {messenger, sent} Message has been sent (but no guarantee)
%%%
%%%
%%% To Client: {message_from, Name, Message},
%%%
%%%
%%% Protocol between the "commands" and the client
%%% ---------------------------------------------------------
%%%
%%% Started: messenger:client(Server_Node, Name)
%%% To client: logoff
%%% To client: {message_to, ToName, Message}
%%%
%%% Configuration: change the server_node() function to return the name of the node where the messenger server runs

-module(chat_server).
-export([start_server/0, server/1]).


%%% return the name of the node messenger server runs
server_node() ->
    chat_server@localhost.

%%% This is the server process for the "messenger"
%%% the user list has the format [{ClientPid, Name}, {ClientPid, Name}...]
server(User_List) ->
    receive 
        {From, logon, Name} ->
            io:format("user ~p logon~n", [Name]),
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List);
        print_users ->
            io:format("Users: ~p~n", [User_List]),
            server(User_List)
    end.

%%% start the server
start_server() ->
    register(messenger, spawn(chat_server, server, [[]])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% Check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            User_List;
        false ->
            From ! { messger, logged_on},
            [{From, Name} | User_List]
    end.

%%% Server deletes a user from the user list
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Server transfers a message between user
server_transfer(From, To, Message, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.
