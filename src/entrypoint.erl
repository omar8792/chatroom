-module(entrypoint).

-export([start/0, pre_loop/1]).

start() ->
    chat_server:boot(),
    socket_server:start(?MODULE, 12345, {?MODULE, pre_loop}).

pre_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_ | Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            io:format("Nick: ~p~n", [Nick]),
            case Command of
                "CONNECT" ->
                    try_connection(clean(Nick), Socket);
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    io:format("USER SOCKER: ~p~n", [Socket]),

    Response = gen_server:call(chat_server, {connect, Nick, Socket}),
    case Response of
        {ok, User} ->
            gen_tcp:send(Socket, "CONNECT:OK:" ++ User ++ "\n"),
            loop(Nick, Socket);
        nickname_already_in_use ->
            gen_tcp:send(Socket, "CONNECT:ERROR:Nickname already in use.\n"),
            ok
    end.

loop(Nick, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_ | _]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            case Command of
                "QUIT" ->
                    disconnect(Nick, Socket);
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

clean(Data) ->
    string:strip(Data, both, $\n).
