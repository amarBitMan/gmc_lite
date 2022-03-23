-module(barcode_change_web).
-compile(export_all).


%% Cowboy handler callbacks
-export([
    init/2,
    terminate/3
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
    case catch (jsx:decode(Body, [return_maps, {labels, atom}])) of
        {'EXIT', _Reason} ->
            Req2 = cowboy_req:reply(400, #{}, Request),
            {ok, Req2, State};
        #{jsons_dir := JsonPathBin} = Payload ->
            JsonPath = erlang:binary_to_list(JsonPathBin),
            case file:list_dir(JsonPath) of
                {ok, []} ->
                    Req1 = cowboy_req:reply(400, #{}, jsx:decode("No Json files"), Req),
                    {ok, Req1, State};
                {ok, JsonFiles} ->
                    BarcodeFmt = maps:get(barcode_fmt, Payload, <<"512X+Y">>),
                    GOMapJson = read_json(JsonPath ++ "/map.json"),
                    DecodeRule =
                        case lists:member("qt_map.json", JsonFiles) of
                            false ->
                                'X*512+Y';
                            true ->
                                read_json(JsonPath ++ "/qt_map.json")
                        end,
                    Response = create_jsons(GOMapJson, DecodeRule, BarcodeFmt, JsonPath, JsonFiles),
                    Req1 = cowboy_req:reply(200, #{}, Response, Req),
                    {ok, Req1, State};
                _Error ->
                    Req1 = cowboy_req:reply(400, #{}, jsx:encode(<<"Dir not found">>), Req),
                    {ok, Req1, State}
            end;
        _WrongPayload ->
            Req1 = cowboy_req:reply(400, #{}, jsx:encode(<<"Wrong Payload">>), Req),
            {ok, Req1, State}
    end;
init(Req0 = #{method := _Method, path := _Path}, State) ->
    Req = cowboy_req:reply(
        404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req0
    ),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

create_jsons(GOMapJson, Rule, BarcodeFmt, JsonPath, JsonFiles) ->
    GOMapJson1 = add_floor_keys(GOMapJson),
    {BarcodeMappingF, GOMapJsonF} =
        lists:foldl(
            fun(
                #{map_values := MapValues} = FloorMap,
                {BarcodeMappingAcc, GOMapJsonAcc}
            ) ->
                {BarcodeMapping, NewMapValues} =
                    apply_barcode_change(MapValues, Rule, BarcodeFmt),
                {
                    maps:merge(BarcodeMappingAcc, BarcodeMapping),
                    GOMapJsonAcc ++
                        [add_vda5050_ref(FloorMap#{map_values => NewMapValues}, Rule, BarcodeFmt)]
                }
            end,
            {#{}, []},
            GOMapJson1
        ),
    Jsons =
        lists:filtermap(
            fun(JsonFileName) ->
                FilePath = JsonPath ++ "/" ++ JsonFileName,
                case filelib:is_dir(FilePath) of
                    false ->
                        case apply_barcode_change(JsonFileName, FilePath, BarcodeMappingF) of
                            [] ->
                                false;
                            NewJson ->
                                {true, {JsonFileName, NewJson}}
                        end;
                    _Dir ->
                        false
                end
            end,
            JsonFiles -- ["map.json", "qt_map.json"]
        ),
    write_files([{"map.json", GOMapJsonF} | Jsons], JsonPath).

add_floor_keys([#{floor_id := _Id} | _RemFloors] = FloorList) ->
    FloorList;
add_floor_keys(FloorList) ->
    [
        #{
            floor_id => 1,
            map_values => FloorList
        }
    ].

apply_barcode_change(MapValues, 'X*512+Y', BarcodeFmt) ->
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
    WorldCoordinatesXSorted = lists:usort(WorldCoordinatesX),
    QTCoordinates =
        [
            {X, Y}
         || X <- lists:seq(0, length(WorldCoordinatesXSorted) - 1),
            Y <- lists:seq(0, length(WorldCoordinatesYSorted) - 1)
        ],
    WorldCoordinates =
        [
            {X, Y}
         || X <- WorldCoordinatesXSorted,
            Y <- WorldCoordinatesYSorted
        ],
    WCToQTCoordMap = maps:from_list(lists:zip(WorldCoordinates, QTCoordinates)),
    lists:foldl(
        fun(
            #{world_coordinate := JsonWorldCoordinate, barcode := OldBarcode} = NodeData,
            {BarcodeMappingAcc2, NewMapValuesAcc}
        ) ->
            [WX, WY] = jsx:decode(JsonWorldCoordinate),
            {BarcodeX, BarcodeY} = maps:get({WX, WY}, WCToQTCoordMap),
            QTBarcode = create_barcode(BarcodeX, BarcodeY, BarcodeFmt),
            {
                BarcodeMappingAcc2#{OldBarcode => QTBarcode},
                NewMapValuesAcc ++ [NodeData#{barcode => QTBarcode}]
            }
        end,
        {#{}, []},
        MapValues
    );
apply_barcode_change(MapValues, #{zoneList := ZoneList}, BarcodeFmt) ->
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
    WorldCoordinatesXSorted = lists:usort(WorldCoordinatesX),
    WorldCoordinates =
        [
            {X, Y}
         || X <- WorldCoordinatesXSorted,
            Y <- WorldCoordinatesYSorted
        ],
    %% QT World coordinates sorting and barcode mapping
    {QTCoordinatesToBarcodeMap, XCoords, YCoords} =
        lists:foldl(
            fun(#{pointList := PointList}, Acc) ->
                lists:foldl(
                    fun(#{barCode := Barcode, x := X, y := Y}, {MapAcc, XCoordsAcc, YCoordsAcc}) ->
                        {MapAcc#{{X, Y} => Barcode}, [X | XCoordsAcc], [Y | YCoordsAcc]}
                    end,
                    Acc,
                    PointList
                )
            end,
            {#{}, [], []},
            ZoneList
        ),
    QTCoordinates =
        [
            {X, Y}
         || X <- lists:usort(XCoords),
            Y <- lists:usort(YCoords)
        ],
    WCToQTCoordMap = maps:from_list(lists:zip(WorldCoordinates, QTCoordinates)),
    lists:foldl(
        fun(
            #{world_coordinate := JsonWorldCoordinate, barcode := OldBarcode} = NodeData,
            {BarcodeMappingAcc, NewMapValuesAcc}
        ) ->
            [WX, WY] = jsx:decode(JsonWorldCoordinate),
            {QTX, QTY} = maps:get({WX, WY}, WCToQTCoordMap),
            QTBarcodeInt = maps:get({QTX, QTY}, QTCoordinatesToBarcodeMap),
            BarcodeX = QTBarcodeInt div 512,
            BarcodeY = QTBarcodeInt rem 512,
            QTBarcode = create_barcode(BarcodeX, BarcodeY, BarcodeFmt),
            {
                BarcodeMappingAcc#{OldBarcode => QTBarcode},
                NewMapValuesAcc ++ [NodeData#{barcode => QTBarcode}]
            }
        end,
        {#{}, []},
        MapValues
    );
apply_barcode_change("pps.json", Path, BarcodeMapping) ->
    lists:map(
        fun(OnePps) ->
            maps:fold(
                fun
                    (location, Barcode, NewJson) ->
                        NewJson#{location => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (queue_barcodes, QueueBarcodes, NewJson) ->
                        NewQueueBarcodes =
                            [
                                maps:get(Barcode, BarcodeMapping, Barcode)
                             || Barcode <- QueueBarcodes
                            ],
                        NewJson#{queue_barcodes => NewQueueBarcodes};
                    (pick_position, Barcode, NewJson) ->
                        NewJson#{pick_position => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (_K, _V, NewJson) ->
                        NewJson
                end,
                OnePps,
                OnePps
            )
        end,
        read_json(Path)
    );
apply_barcode_change("charger.json", Path, BarcodeMapping) ->
    lists:map(
        fun(OneCharger) ->
            maps:fold(
                fun
                    (charger_location, Barcode, NewJson) ->
                        NewJson#{charger_location => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (entry_point_location, Barcode, NewJson) ->
                        NewJson#{
                            entry_point_location => maps:get(Barcode, BarcodeMapping, Barcode)
                        };
                    (reinit_point_location, Barcode, NewJson) ->
                        NewJson#{
                            reinit_point_location => maps:get(Barcode, BarcodeMapping, Barcode)
                        };
                    (_K, _V, NewJson) ->
                        NewJson
                end,
                OneCharger,
                OneCharger
            )
        end,
        read_json(Path)
    );
apply_barcode_change("fire_emergency.json", Path, BarcodeMapping) ->
    lists:map(
        fun(OneGroup) ->
            maps:fold(
                fun
                    (barcode, Barcode, NewJson) ->
                        NewJson#{barcode => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (_K, _V, NewJson) ->
                        NewJson
                end,
                OneGroup,
                OneGroup
            )
        end,
        read_json(Path)
    );
apply_barcode_change("racks.json", Path, BarcodeMapping) ->
    lists:map(
        fun(OneRack) ->
            maps:fold(
                fun
                    (position, Barcode, NewJson) ->
                        NewJson#{position => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (last_store_position, Barcode, NewJson) ->
                        NewJson#{position => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (_K, _V, NewJson) ->
                        NewJson
                end,
                OneRack,
                OneRack
            )
        end,
        read_json(Path)
    );
apply_barcode_change("elevator.json", Path, BarcodeMapping) ->
    lists:map(
        fun(OneElevator) ->
            maps:fold(
                fun
                    (MultiBarcodeKey, Barcodes, NewJson) when
                        MultiBarcodeKey == reinit_barcodes orelse
                            MultiBarcodeKey == exit_barcodes orelse
                            MultiBarcodeKey == entry_barcodes
                    ->
                        NewBarcodes =
                            lists:map(
                                fun(#{barcode := Barcode} = Data) ->
                                    Data#{barcode => maps:get(Barcode, BarcodeMapping, Barcode)}
                                end,
                                Barcodes
                            ),
                        NewJson#{MultiBarcodeKey => NewBarcodes};
                    (position, Barcode, NewJson) ->
                        NewJson#{position => maps:get(Barcode, BarcodeMapping, Barcode)};
                    (_K, _V, NewJson) ->
                        NewJson
                end,
                OneElevator,
                OneElevator
            )
        end,
        read_json(Path)
    );
apply_barcode_change(_, _, _) ->
    [].

add_vda5050_ref(
    #{
        map_values := [
            #{barcode := Barcode} | _RemNodes
        ]
    } = FloorMap,
    'X*512+Y',
    _BarcodeFmt
) ->
    RefMap =
        #{
            barcode => Barcode,
            vda5050_coordinate => jsx:encode([0, 0])
        },
    FloorMap#{vda5050_reference => RefMap};
add_vda5050_ref(
    FloorMap,
    #{
        zoneList := [
            #{
                pointList := [
                    #{
                        barCode := Barcode, x := X, y := Y
                    }
                    | _RemPointList
                ]
            }
            | _RemZoneList
        ]
    },
    BarcodeFmt
) ->
    RefMap =
        #{
            barcode => create_barcode(Barcode div 512, Barcode rem 512, BarcodeFmt),
            vda5050_coordinate => jsx:encode([X, Y])
        },
    FloorMap#{vda5050_reference => RefMap}.

to_binary_coord(Coord) when Coord >= 100 ->
    erlang:integer_to_list(Coord);
to_binary_coord(Coord) when Coord >= 10 ->
    "0" ++ erlang:integer_to_list(Coord);
to_binary_coord(Coord) ->
    "00" ++ erlang:integer_to_list(Coord).

create_barcode(X, Y, <<"512X+Y">>) ->
    integer_to_binary(512 * X + Y);
create_barcode(X, Y, _CoordFmt) ->
    erlang:list_to_binary([
        to_binary_coord(X),
        <<".">>,
        to_binary_coord(Y)
    ]).

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.

read_json(Path) ->
    {ok, File} = file:open(Path, [read]),
    {ok, Json} = file:read(File, 100 * 1024 * 1024),
    jsx:decode(erlang:list_to_binary(Json), [return_maps, {labels, atom}]).

prettify_json(JsonableTerm) ->
    jsx:encode(JsonableTerm).
%%jsx:prettify(jsx:encode(JsonableTerm, [{indent, 2}, {space, 1}])).

write_files(Jsons, Dir) ->
    NewDir = Dir ++ "/updated_jsons",
    case file:list_dir(NewDir) of
        {ok, _Files} ->
            ok;
        _ ->
            ok = file:make_dir(NewDir)
    end,
    lists:foreach(
        fun({JsonFileName, JsonData}) ->
            PrettyJsonData = prettify_json(JsonData),
            FilePath = NewDir ++ "/" ++ JsonFileName,
            ok = file:write_file(FilePath, PrettyJsonData, [])
        end,
        Jsons
    ),
    jsx:encode(#{new_jsons_dir => list_to_binary(NewDir)}).
