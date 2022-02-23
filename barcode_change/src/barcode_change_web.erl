-module(barcode_change_web).
-export([
    start/0,
    start/1,
    stop/0,
    stop/1
]).

%% Cowboy handler callbacks
-export([
    init/2,
    terminate/3,
    add_floor_keys/1
]).

start() ->
    start(#{}).

start(InputOpts) ->
    {ok, ConfOpts} = application:get_env(barcode_change, http_opts),
    #{
        port := Port,
        listener_name := ListenerName
    } = maps:merge(ConfOpts, InputOpts),
    Dispatch = cowboy_router:compile([
        {'_', [{"/api/[...]", barcode_change_web, []}]}
    ]),
    {ok, _} = cowboy:start_clear(
        ListenerName,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ListenPort = ranch:get_port(ListenerName),
    {ok, ListenPort}.

stop() ->
    stop(barcode_change_http_listener).

stop(ListenerName) ->
    cowboy:stop_listener(ListenerName).

init(
    Req = #{method := <<"POST">>, path := <<"/api/change_barcode">>},
    State
) ->
    {ok, Body, Request} = read_body(Req),
    case catch(jsx:decode(Body, [return_maps, {labels, atom}])) of
        {'EXIT', _Reason} ->
            Req2 = cowboy_req:reply(400, #{}, Request),
            {ok, Req2, State};
        #{gmc_map_path := GmcMapPath, qt_map_path := QtMapPath}  ->
            {ok, F1} = read_json(GmcMapPath),
            {ok, F2} = read_json(QtMapPath),
            GmcMapJson = jsx:decode(erlang:list_to_binary(F1), [return_maps, {labels, atom}]),
            QtMapJson = jsx:decode(erlang:list_to_binary(F2), [return_maps, {labels, atom}]),
            NewJson = create_json(GmcMapJson, QtMapJson),
            [Dir, _FileName] = string:split(binary_to_list(GmcMapPath), "/", trailing),
            MapFilePath = Dir ++ "/updated_map.json",
            file:write_file(MapFilePath, NewJson, []),
            Req1 = cowboy_req:reply(200, #{}, jsx:encode(#{newJsonPath => list_to_binary(MapFilePath)}), Req),
            {ok, Req1, State}; 
        #{gmc_map_path := GmcMapPath}  ->
            {ok, F1} = read_json(GmcMapPath),
            GmcMapJson = jsx:decode(erlang:list_to_binary(F1), [return_maps, {labels, atom}]),
            NewJson = create_json(GmcMapJson),
            [Dir, _FileName] = string:split(binary_to_list(GmcMapPath), "/", trailing),
            MapFilePath = Dir ++ "/updated_map.json",
            file:write_file(MapFilePath, NewJson, []),
            Req1 = cowboy_req:reply(200, #{}, jsx:encode(#{newJsonPath => list_to_binary(MapFilePath)}), Req),
            {ok, Req1, State}; 
        #{gmc_map := GmcMapJson, qt_map := QtMapJson} ->
            NewJson = create_json(GmcMapJson, QtMapJson),
            Req1 = cowboy_req:reply(200, #{}, NewJson, Req),
            {ok, Req1, State};  
        #{gmc_map := GmcMapJson} ->
            NewJson = create_json(GmcMapJson),
            Req1 = cowboy_req:reply(200, #{}, NewJson, Req),
            {ok, Req1, State}       
    end;
init(Req0 = #{method := _Method, path := _Path}, State) ->
    Req = cowboy_req:reply(
        404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req0
    ),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

create_json(GmcMapJson) ->
    GmcMapJson1 = add_floor_keys(GmcMapJson),
    GmcMapJson2 =
        lists:map(
            fun(#{map_values := MapValues} = FloorMap) ->
                NewMapValues = apply_barcode_change(MapValues, 'X*512+Y'),
                FloorMap#{map_values =>  NewMapValues}
            end,
            GmcMapJson1
        ),
    jsx:prettify(jsx:encode(GmcMapJson2, [{indent, 2}, {space, 1}])).

create_json(GmcMapJson, QtMapJson) ->
    GmcMapJson1 = add_floor_keys(GmcMapJson),
    GmcMapJson2 =
        lists:map(
            fun(#{map_values := MapValues} = FloorMap) ->
                NewMapValues = apply_barcode_change(MapValues, QtMapJson),
                FloorMap#{map_values =>  NewMapValues}
            end,
            GmcMapJson1
        ),
    jsx:prettify(jsx:encode(GmcMapJson2, [{indent, 2}, {space, 1}])).

add_floor_keys([#{floor_id := _Id} | _RemFloors] = FloorList) ->
    FloorList;
add_floor_keys(FloorList) ->
    [
        #{
            floor_id => 1,
            map_values => FloorList
        }
    ].

apply_barcode_change(MapValues, 'X*512+Y') ->
    WorldCoordinateList =
        lists:map(
            fun(#{world_coordinate := JsonWorldCoordinate}) ->
                WorldCoordinateList = jsx:decode(JsonWorldCoordinate),
                list_to_tuple(WorldCoordinateList)
            end,
            MapValues
        ),
    {WorldCoordinatesX, WorldCoordinatesY} = lists:unzip(WorldCoordinateList),
    WorldCoordinatesYSorted = lists:reverse(lists:usort(WorldCoordinatesY)),
    WorldCoordinatesXSorted= lists:usort(WorldCoordinatesX),
    QTCoordinates =
        [
            {X, Y} ||
            X <- lists:seq(1, length(WorldCoordinatesXSorted)),
            Y <- lists:seq(1, length(WorldCoordinatesYSorted))
        ],
    WorldCoordinates =
        [
            {X, Y} ||
            X <- WorldCoordinatesXSorted,
            Y <- WorldCoordinatesYSorted
        ],
    WCToQTCoordMap = maps:from_list(lists:zip(WorldCoordinates, QTCoordinates)),
    NewMapValues =
        lists:map(
            fun(#{world_coordinate := JsonWorldCoordinate} = NodeData) ->
                [WX, WY] = jsx:decode(JsonWorldCoordinate),
                {QTX, QTY} = maps:get({WX, WY}, WCToQTCoordMap),
                QTBarcode = erlang:integer_to_binary(512*QTX + QTY),
                NodeData#{barcode => QTBarcode}
            end,
            MapValues
        ), 
    NewMapValues;
apply_barcode_change(MapValues, #{zoneList := ZoneList}) ->
    %% GMC World coordinates sorting
    WorldCoordinateList =
        lists:map(
            fun(#{world_coordinate := JsonWorldCoordinate}) ->
                WorldCoordinateList = jsx:decode(JsonWorldCoordinate),
                list_to_tuple(WorldCoordinateList)
            end,
            MapValues
        ),
    {WorldCoordinatesX, WorldCoordinatesY} = lists:unzip(WorldCoordinateList),
    WorldCoordinatesYSorted = lists:reverse(lists:usort(WorldCoordinatesY)),
    WorldCoordinatesXSorted= lists:usort(WorldCoordinatesX),
    WorldCoordinates =
        [
            {X, Y} ||
            X <- WorldCoordinatesXSorted,
            Y <- WorldCoordinatesYSorted
        ],
    %% QT World coordinates sorting and barcode mapping
    {QTCoordinatesToBarcodeMap, XCoords, YCoords} =
        lists:foldl(
            fun(#{pointList := PointList}, Acc) ->
                lists:foldl(
                    fun(#{barCode := Barcode, x:= X, y := Y}, {MapAcc, XCoordsAcc, YCoordsAcc}) ->
                        {MapAcc#{{X, Y} => Barcode}, [X|XCoordsAcc], [Y|YCoordsAcc]}
                    end,
                    Acc, PointList
                ) 
            end,
            {#{}, [], []}, ZoneList
        ),
    QTCoordinates =
        [
            {X, Y} ||
            X <- lists:usort(XCoords),
            Y <- lists:usort(YCoords)
        ],
    io:format("QTCoordinates = ~p~n", [QTCoordinates]),
    io:format("WorldCoordinates = ~p~n", [WorldCoordinates]),
    WCToQTCoordMap = maps:from_list(lists:zip(WorldCoordinates, QTCoordinates)),
    NewMapValues =
        lists:map(
            fun(#{world_coordinate := JsonWorldCoordinate} = NodeData) ->
                [WX, WY] = jsx:decode(JsonWorldCoordinate),
                {QTX, QTY} = maps:get({WX, WY}, WCToQTCoordMap),
                QTBarcode = maps:get({QTX, QTY}, QTCoordinatesToBarcodeMap),
                NodeData#{barcode => erlang:integer_to_binary(QTBarcode)}
            end,
            MapValues
        ), 
    NewMapValues.

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

read_json(Path) ->
    {ok, File} = file:open(Path,[read]),
    file:read(File,1024 * 1024).