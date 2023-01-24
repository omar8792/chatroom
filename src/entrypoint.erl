-module(entrypoint).

-export([start/0, pre_loop/1]).

start() ->
    chat_server:boot(),
    socket_server:start(?MODULE, 12345, {?MODULE, pre_loop}).

pre_loop(Socket) ->
    gen_tcp:send(Socket, "Welcome, please type a username to connect to the chat\n"),

    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Nick = binary_to_list(Data),
            io:format("Nick: ~p~n", [Nick]),
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
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_ | Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Command of
                "QUIT" ->
                    disconnect(Nick, Socket);
                "SAY" ->
                    say(Nick, Socket, clean(Content));
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

disconnect(Nick, Socket) ->
    Response = gen_server:call(chat_server, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye.\n"),
            gen_server:cast(chat_server, {left, Nick}),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors.\n"),
            ok
    end.

say(Nick, Socket, Content) ->
    gen_server:cast(chat_server, {say, Nick, Content}),
    loop(Nick, Socket).

clean(Data) ->
    re:replace(Data, "[\r\n]+", "", [global, {return, list}]).
