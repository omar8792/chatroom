-module(socket_server).

-behavior(gen_server).

-export([init/1]).
-export([accept_loop/1, handle_cast/2, handle_call/3, accept/1]).
-export([start/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
    port,
    loopFn,
    socket = null
}).

start(Name, Port, LoopFn) ->
    State = #server_state{port = Port, loopFn = LoopFn},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port = Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, Socket} ->
            NewState = State#server_state{socket = Socket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State = #server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, Socket, {Module, LoopFunction}}) ->
    {ok, TcpSocket} = gen_tcp:accept(Socket),
    gen_server:cast(Server, {accepted, self()}),

    Module:LoopFunction(TcpSocket).

accept(State = #server_state{socket = Socket, loopFn = LoopFn}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), Socket, LoopFn}]),
    State.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
