-module(chat_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(user, {nickname, connected_at, msg_sent = 0}).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    State = dict:new(),
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    Return.

handle_call({connect, Nickname}, _From, State) ->
    Response =
        case dict:is_key(Nickname, State) of
            true ->
                NewState = State,
                nickname_already_in_use;
            false ->
                NewState = dict:append(
                    Nickname, #user{nickname = Nickname, connected_at = erlang:localtime()}, State
                ),
                ConnectedUsers = string:join(dict:fetch_keys(NewState), ":"),
                {ok, ConnectedUsers}
        end,

    Return = {reply, Response, NewState},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call({disconnect, Nickname}, _From, State) ->
    Response =
        case dict:is_key(Nickname, State) of
            true ->
                NewState = dict:erase(Nickname, State),
                ok;
            false ->
                NewState = State,
                nickname_not_found
        end,

    Return = {reply, Response, NewState},
    io:format("handle_call: ~p~n", [Return]),
    Return;
handle_call(_Message, _From, State) ->
    {reply, unknown_message, State}.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

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
