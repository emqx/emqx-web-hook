-module(http_server).

-compile(export_all).

%%%%%%%start http listen%%%%%%%%%%%%%%%%%%%%%
start_http() ->
    %process_flag(trap_exit, true),
    io:format("start http~n", []),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
              {"/", ?MODULE, []}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_clear(http, [{port, 8991}], #{
        env => #{dispatch => Dispatch}
    }).

stop_http() ->
    cowboy:stop_listener(http).

init(Req, Opts) ->
    io:format("init Req: ~p~n", [Req]),
    Req1 = handle_request(Req),
    {ok, Req1, Opts}.

handle_request(Req) ->
    Method =cowboy_req:method(Req),
    Params =
        case Method of
            <<"GET">> -> cowboy_req:parse_qs(Req);
            <<"POST">> ->
                {ok, PostVals, _Req2} = cowboy_req:read_urlencoded_body(Req),
                PostVals
        end,

    io:format("Method: ~p, Param: ~p", [Method, Params]),
    reply(Req, ok).

reply(Req, ok) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"hello">>, Req);
reply(Req, error) ->
    cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"deny">>, Req).
