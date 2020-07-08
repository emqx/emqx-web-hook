%%--------------------------------------------------------------------
%% A Simple HTTP Server based cowboy
%%
%% It will deliver the http-request params to initialer process
%%--------------------------------------------------------------------
-module(http_server).

-compile(export_all).
-compile(nowarn_export_all).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_http() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
              {"/", ?MODULE, self()}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Start http server on 8080 successfully!~n").

stop_http() ->
    ok = cowboy:stop_listener(http),
    io:format("Stopped http server on 8080").

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

init(Req, ReceiverPid) ->
    Req1 = handle_request(Req, ReceiverPid),
    {ok, Req1, ReceiverPid}.

%% @private
handle_request(Req, ReceiverPid) ->
    Method = cowboy_req:method(Req),
    Headers = cowboy_req:headers(Req),
    Params =
        case Method of
            <<"GET">> -> cowboy_req:parse_qs(Req);
            <<"POST">> ->
                {ok, PostVals, _Req2} = cowboy_req:read_urlencoded_body(Req),
                PostVals
        end,
    io:format("Request Data:~p~nHeaders :~p~n", [Params, Headers]),
    erlang:send(ReceiverPid, {Params, Headers}),
    reply(Req, ok).

%% @private
reply(Req, ok) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"hello">>, Req);
reply(Req, error) ->
    cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"deny">>, Req).
