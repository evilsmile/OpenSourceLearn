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

-module(chat_client).
-export([logon/1, logoff/0, message/2, client/2]).


%%% return the name of the node messenger server runs
server_node() ->
    chat_server@localhost.

logon(Name) ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client,
                     spawn(chat_client, client, [server_node(), Name]));
        _ ->
            already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of 
        undefined ->
            not_logged_on;
        _ -> mess_client ! {message_to, ToName, Message},
             ok
    end.

client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff };
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} -> 
            io:format("~p~n", [What])
    end.

