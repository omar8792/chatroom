## Chatroom
A very simple chat server written in Erlang.
This is an exercise to learn a bit about the language.
## How can I run it?
build and run the container with docker compose
> `docker compose build`

> `docker compose run web bash --service-ports`

Then run the server with rebar3 in interactive shell mode
> `rebar3 shell` 

The server is not up and running listening to incoming tcp connection at the port `12345`

## How can I interact with the server?
You can interact with the server sending via TCP known messages.

Eg: 
`sh telnet localhost 12345` to connect

Then you can interact with known message formats

