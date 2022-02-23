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
    case catch(jsx:decode(Body, [return_maps, {labels, atom}]))of
        {'EXIT', _Reason} ->
            Req2 = cowboy_req:reply(400, #{}, Request),
            {ok, Req2, State};
        #{gmc_map := GmcMapJson, qt_map := QtMapJson} ->
            GmcMapJson1 = add_floor_keys(GmcMapJson),
            GmcMapJson2 =
                lists:map(
                    fun(#{map_values := MapValues} = FloorMap) ->
                        NewMapValues = apply_barcode_change(MapValues, QtMapJson),
                        FloorMap#{map_values =>  NewMapValues}
                    end,
                    GmcMapJson1
                ),
            NewJson = jsx:prettify(jsx:encode(GmcMapJson2, [{indent, 2}, {space, 1}])),
            Req1 = cowboy_req:reply(200, #{}, NewJson, Req),
            {ok, Req1, State};  
        #{gmc_map := GmcMapJson} ->
            GmcMapJson1 = add_floor_keys(GmcMapJson),
            GmcMapJson2 =
                lists:map(
                    fun(#{map_values := MapValues} = FloorMap) ->
                        NewMapValues = apply_barcode_change(MapValues, 'X*512+Y'),
                        FloorMap#{map_values =>  NewMapValues}
                    end,
                    GmcMapJson1
                ),
            NewJson = jsx:prettify(jsx:encode(GmcMapJson2, [{indent, 2}, {space, 1}])),
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
    WCToQTCoordMap = maps:from_list(lists:zip(WorldCoordinates, QTCoordinates)),
    NewMapValues =
        lists:map(
            fun(#{world_coordinate := JsonWorldCoordinate} = NodeData) ->
                [WX, WY] = jsx:decode(JsonWorldCoordinate),
                {QTX, QTY} = maps:get({WX, WY}, WCToQTCoordMap),
                QTBarcode = maps:get({QTX, QTY}, QTCoordinatesToBarcodeMap),
                NodeData#{barcode => QTBarcode}
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