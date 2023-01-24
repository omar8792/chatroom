-module(chat_server).
-behaviour(gen_server).
-export([boot/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(user, {nickname, socket, connected_at, msg_sent = 0}).
-record(room, {name, users = [], created_by, created_at}).

boot() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    UsersState = dict:new(),
    RoomsState = dict:new(),
    Return = {ok, {UsersState, RoomsState}},
    io:format("init: ~p~n", [{UsersState, RoomsState}]),
    Return.

handle_call({connect, Nickname, UserSocket}, _From, {UsersState, RoomsState}) ->
    Response =
        case dict:is_key(Nickname, UsersState) of
            true ->
                NewUsersState = UsersState,
                nickname_already_in_use;
            false ->
                broadcast("#", Nickname ++ " has connected\n", UsersState),

                NewUsersState = dict:append(
                    Nickname,
                    #user{
                        nickname = Nickname,
                        socket = UserSocket,
                        connected_at = erlang:localtime()
                    },
                    UsersState
                ),
                {ok, Nickname}
        end,

    Return = {reply, Response, {NewUsersState, RoomsState}},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call({disconnect, Nickname}, _From, {UsersState, RoomsState}) ->
    Response =
        case dict:is_key(Nickname, UsersState) of
            true ->
                NewUsersState = dict:erase(Nickname, UsersState),
                ok;
            false ->
                NewUsersState = UsersState,
                nickname_not_found
        end,

    Return = {reply, Response, {NewUsersState, RoomsState}},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call({create_room, Nickname, RoomName}, _From, {UsersState, RoomsState}) ->
    Response =
        case dict:is_key(RoomName, RoomsState) of
            true ->
                NewRoomsState = RoomsState,
                room_already_exists;
            false ->
                NewRoomsState = dict:append(
                    RoomName,
                    #room{name = RoomName, created_by = Nickname, created_at = erlang:localtime()},
                    RoomsState
                ),
                AvailableRooms = string:join(dict:fetch_keys(NewRoomsState), ":"),
                {ok, {UsersState, AvailableRooms}}
        end,
    Return = {reply, Response, {UsersState, NewRoomsState}},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call({delete_room, Nickname, RoomName}, _From, {UsersState, RoomsState}) ->
    Response =
        case dict:find(RoomName, RoomsState) of
            {ok, [R | _]} ->
                if
                    R#room.created_by == Nickname ->
                        NewRoomsState = dict:erase(RoomName, RoomsState),
                        ok;
                    true ->
                        NewRoomsState = RoomsState,
                        room_not_created_by_user
                end;
            _ ->
                NewRoomsState = RoomsState,
                room_not_found
        end,
    Return = {reply, Response, {UsersState, NewRoomsState}},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call(_Message, _From, State) ->
    {reply, unknown_message, State}.

handle_cast({list_rooms, UserSocket}, {_UsersState, RoomsState} = State) ->
    direct(UserSocket, "available rooms:\n" ++ string:join(dict:fetch_keys(RoomsState), ":")),
    {noreply, State};
handle_cast({say, Nick, Msg}, {UsersState, _RoomsState} = State) ->
    broadcast(Nick, Nick ++ ": " ++ Msg ++ "\n", UsersState),
    {noreply, State};
handle_cast(Message, State) ->
    io:format("handle_cast unknown message received: ~p~n", [Message]),
    {noreply, State}.

handle_info(_Info, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.

broadcast(Nick, Msg, Users) ->
    Sockets = lists:map(
        fun({_, [User | _]}) -> User#user.socket end, dict:to_list(dict:erase(Nick, Users))
    ),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

direct(UserSocket, Msg) ->
    gen_tcp:send(UserSocket, Msg).
