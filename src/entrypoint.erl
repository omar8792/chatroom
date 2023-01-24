-module(entrypoint).

-export([start/0, pre_loop/1]).

start() ->
    chat_server:boot(),
    socket_server:start(?MODULE, 12345, {?MODULE, pre_loop}).

pre_loop(Socket) ->
    gen_tcp:send(
        Socket,
        "Welcome\n\n"
        "Available commands:\n\n"
        "- SAY:<something> to send a message to all connected users\n"
        "- CREATE_ROOM:<room_name> to create a room\n"
        "- DESTROY_ROOM:<something> to delete a created room\n"
        "- LIST_ROOMS to see available rooms\n"
        "- QUIT to logout\n\n"
        "Please type a username to connect to the chat\n"
    ),

    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Nick = binary_to_list(Data),
            try_connection(clean(Nick), Socket);
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(chat_server, {connect, Nick, Socket}),
    case Response of
        {ok, User} ->
            gen_tcp:send(Socket, User ++ " connected to the chat\n"),
            loop(Nick, Socket);
        nickname_already_in_use ->
            gen_tcp:send(Socket, "Sorry the Nickname " ++ Nick ++ " is already in use.\n"),
            pre_loop(Socket)
    end.

loop(Nick, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary_to_list(Data),
            {Command, Args} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case {clean(Command), Args} of
                {"QUIT", _} ->
                    disconnect(Nick, Socket);
                {"SAY", [_ | Content]} ->
                    say(Nick, Socket, clean(Content));
                {"CREATE_ROOM", [_ | RoomName]} ->
                    case create_room(Nick, clean(RoomName)) of
                        ok -> gen_tcp:send(Socket, "Room created\n");
                        room_already_exists -> gen_tcp:send(Socket, "This room already exists\n")
                    end,
                    loop(Nick, Socket);
                {"DESTROY_ROOM", [_ | RoomName]} ->
                    case delete_room(Nick, clean(RoomName)) of
                        ok -> gen_tcp:send(Socket, "Room deleted\n");
                        room_not_found -> gen_tcp:send(Socket, "This room does not exists\n")
                    end,
                    loop(Nick, Socket);
                {"LIST_ROOMS", _} ->
                    list_rooms(Nick, Socket);
                {_, _} ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    loop(Nick, Socket)
            end;
        {error, closed} ->
            ok
    end.

disconnect(Nick, Socket) ->
    Response = gen_server:call(chat_server, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye.\n"),
            say(Nick, Socket, " user disconnected"),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors.\n"),
            ok
    end.

say(Nick, Socket, Content) ->
    gen_server:cast(chat_server, {say, Nick, Content}),
    loop(Nick, Socket).

create_room(Nick, RoomName) ->
    gen_server:call(chat_server, {create_room, Nick, RoomName}).
delete_room(Nick, RoomName) ->
    gen_server:call(chat_server, {delete_room, Nick, RoomName}).
list_rooms(Nick, Socket) ->
    gen_server:cast(chat_server, {list_rooms, Socket}),
    loop(Nick, Socket).

clean(Data) ->
    re:replace(Data, "[\r\n]+", "", [global, {return, list}]).
