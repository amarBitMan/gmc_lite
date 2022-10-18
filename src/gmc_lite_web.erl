-module(gmc_lite_web).
-export([
    start/0,
    start/1,
    init/2,
    stop/0,
    stop/1,
    terminate/3
]).

%%%=============================================================================
%%% Cowboy handler callbacks
%%%=============================================================================
start() ->
    start(#{}).

start(InputOpts) ->
    {ok, ConfOpts} = application:get_env(gmc_lite, http_opts),
    #{
        port := Port,
        listener_name := ListenerName
    } = maps:merge(ConfOpts, InputOpts),
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", gmc_lite_web, []}]}
    ]),
    {ok, _} = cowboy:start_clear(
        ListenerName,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ListenPort = ranch:get_port(ListenerName),
    {ok, ListenPort}.

stop() ->
    stop(gmc_lite_http_listener).

stop(ListenerName) ->
    cowboy:stop_listener(ListenerName).

init(
    Req = #{method := <<"POST">>},
    State
) ->
    {ok, Body, Req1} = read_body(Req),
    case catch (jsx:decode(Body, [return_maps, {labels, atom}])) of
        {'EXIT', _Reason} ->
            Req2 = cowboy_req:reply(400, #{}, jsx:encode(<<"Invalid Json Format">>), Req1),
            {ok, Req2, State};
        Payload ->
            case handle_post(Req1, Payload) of
                {ok, Response} ->
                    Req2 = cowboy_req:reply(200, #{}, jsx:encode(Response), Req),
                    {ok, Req2, State};
                {error, Response} ->
                    Req2 = cowboy_req:reply(400, #{}, jsx:encode(Response), Req),
                    {ok, Req2, State}
            end
    end;
init(
    Req = #{method := <<"GET">>},
    State
) ->
    Req1 = handle_get(Req),
    {ok, Req1, State}.

handle_get(Req) ->
    cowboy_req:reply(200, #{}, jsx:encode(<<"Get API working">>), Req).

handle_post(
    #{path := <<"/api/change_barcode">>},
    #{jsons_dir := SubDirBin} = PayLoad
) ->
    DataDir = gmc_lite_file_utils:get_data_dir(),
    FullPath = filename:join(DataDir, erlang:binary_to_list(SubDirBin)),
    gmc_lite_barcode_change:apply_barcode_change(PayLoad#{jsons_dir => FullPath});
handle_post(
    #{path := <<"/api/change_barcode">>},
    _PayLoad
) ->
    {error, jsx:encode(<<"Wrong Payload">>)};
handle_post(
    #{path := <<"/api/validate_change_barcode_data">>},
    #{jsons_dir := SubDirBin}
) ->
    DataDir = gmc_lite_file_utils:get_data_dir(),
    FullPath = filename:join(DataDir, erlang:binary_to_list(SubDirBin)),
    gmc_lite_barcode_change:validate(FullPath);
handle_post(
    #{path := <<"/api/generate_rack_json">>},
    #{file_dir := SubDirBin} = PayLoad
) ->
    DataDir = gmc_lite_file_utils:get_data_dir(),
    FullPath = filename:join(DataDir, erlang:binary_to_list(SubDirBin)),
    gmc_lite_json_generator:generate_rack_json(PayLoad#{file_dir => FullPath});
handle_post(
    #{path := <<"/api/generate_rack_json">>},
    _PayLoad
) ->
    {error, <<"Wrong Payload">>};
handle_post(
    _Path,
    _PayLoad
) ->
    {error, <<"Method Not Found">>}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.