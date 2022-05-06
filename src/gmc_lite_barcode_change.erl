-module(gmc_lite_barcode_change).
-compile(export_all).
-compile(nowarn_export_all).

apply_barcode_change(#{jsons_dir := JsonPathBin} = PayLoad) ->
    JsonPath = erlang:binary_to_list(JsonPathBin),
    case file:list_dir(JsonPath) of
        {ok, []} ->
            {error, "Folder Empty"};
        {ok, JsonFiles} ->
            BarcodeFmt = maps:get(barcode_fmt, PayLoad, <<"512X+Y">>),
            GOMapJson = gmc_lite_file_utils:read_json(JsonPath ++ "/map.json"),
            IsQtMap = lists:member("qt_map.json", JsonFiles),
            IsCsvFile = lists:member("qt_barcode_data.csv", JsonFiles),
            DecodeRule =
                if
                    IsQtMap ->
                        gmc_lite_file_utils:read_json(JsonPath ++ "/qt_map.json");
                    IsCsvFile ->
                        BinaryRule = gmc_lite_file_utils:parse_csv_file(JsonPath ++ "/qt_barcode_data.csv"),
                        [[binary_to_integer(Barcode), binary_to_integer(X), binary_to_integer(Y)] || [Barcode, X, Y] <- BinaryRule];
                    true ->
                        'X*512+Y'
                end,
            {ok, transform_jsons(GOMapJson, DecodeRule, BarcodeFmt, JsonPath, JsonFiles)};
        _Error ->
            {error, <<"Dir not found">>}
    end.

transform_jsons(GOMapJson, Rule, BarcodeFmt, JsonPath, JsonFiles) ->
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
            JsonFiles -- ["map.json", "qt_map.json", "qt_barcode_data.csv"]
        ),
    gmc_lite_file_utils:write_files([{"map.json", GOMapJsonF} | Jsons], 
        JsonPath ++ "/updated_jsons"
    ).

add_floor_keys([#{floor_id := _Id} | _RemFloors] = FloorList) ->
    FloorList;
add_floor_keys(FloorList) ->
    [
        #{
            floor_id => 1,
            map_values => FloorList
        }
    ].

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
        gmc_lite_file_utils:read_json(Path)
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
        gmc_lite_file_utils:read_json(Path)
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
        gmc_lite_file_utils:read_json(Path)
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
        gmc_lite_file_utils:read_json(Path)
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
        gmc_lite_file_utils:read_json(Path)
    );
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
    apply_barcode_change(MapValues, ZoneList, BarcodeFmt);
apply_barcode_change(MapValues, ZoneList, BarcodeFmt) when is_list(ZoneList)->
    %% GMC World coordinates sorting
    {
        {_GOCoordinatesToBarcodeMap, GOCoordinateOrderedList}, 
        {QTCoordinatesToBarcodeMap, QTCoordinateOrderedList}
    } = get_go_qt_coords(MapValues, ZoneList),
    WCToQTCoordMap = maps:from_list(lists:zip(GOCoordinateOrderedList, QTCoordinateOrderedList)),
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
    FloorMap#{vda5050_reference => RefMap};

add_vda5050_ref(
    FloorMap,
    [[Barcode, X, Y] | _Rem],
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

get_go_qt_coords(MapValues, BarcodeData) ->
    GOCoordinatesToBarcodeMap =
        lists:foldl(
            fun(#{world_coordinate := JsonWorldCoordinate, barcode := GOBarcode}, MapAcc) ->
                WorldCoordinateList = jsx:decode(JsonWorldCoordinate),
                MapAcc#{list_to_tuple(WorldCoordinateList) => GOBarcode}
            end,
           #{},  MapValues
        ),
    GOCoordinateOrderedList =
        lists:sort(
            fun({X1, Y1}, {X2, Y2}) ->
                X1 < X2 orelse (X1 == X2 andalso Y1 > Y2)
            end,
            maps:keys(GOCoordinatesToBarcodeMap)
        ),
    {
        {GOCoordinatesToBarcodeMap, GOCoordinateOrderedList},
        get_qt_coords(BarcodeData)
    }.

get_qt_coords(BarcodeData) ->
    QTCoordinatesToBarcodeMap =
        lists:foldl(
            fun
                (#{pointList := PointList}, Acc) ->
                    lists:foldl(
                        fun(#{barCode := Barcode, x := X, y := Y}, MapAcc) ->
                            MapAcc#{{X, Y} => Barcode}
                        end,
                        Acc,
                        PointList
                    );
                ([Barcode, X, Y], MapAcc) ->
                    MapAcc#{{X, Y} => Barcode}
            end,
            #{},
            BarcodeData
        ),
    QTCoordinateOrderedList = lists:usort(maps:keys(QTCoordinatesToBarcodeMap)),
    {QTCoordinatesToBarcodeMap, QTCoordinateOrderedList}.

validate(JsonPath) ->
    GOMapJson = gmc_lite_file_utils:read_json(JsonPath ++ "/map.json"),
    QTMapJson = gmc_lite_file_utils:read_json(JsonPath ++ "/qt_map.json"),
    #{zoneList := ZoneList} = QTMapJson,
    [#{map_values := MapValues} | _] = add_floor_keys(GOMapJson),
    case validate_internal(MapValues, ZoneList) of
        {false, coordinate_count_diff, GoCount, QTCount} ->
            {error,
                #{
                    go_coord_count => GoCount,
                    qt_coord_count => QTCount
                }
            };
        {false, NotMatchedCoords, GOCoordinatesToBarcodeMap, QTCoordinatesToBarcodeMap} ->
            ErrorDetails =
                lists:map(
                    fun({{G1, Q1}, {G2, Q2}}) ->
                        #{
                            go_map => #{
                                go_barcode_src => maps:get(G1, GOCoordinatesToBarcodeMap),
                                go_coord_src => jsx:encode(tuple_to_list(G1)) ,
                                go_barcode_dest => maps:get(G2, GOCoordinatesToBarcodeMap),
                                go_coord_dest => jsx:encode(tuple_to_list(G2))
                            },
                            qt_map => #{
                                qt_barcode_src => integer_to_binary(maps:get(Q1, QTCoordinatesToBarcodeMap)),
                                qt_coord_src => jsx:encode(tuple_to_list(Q1)),
                                qt_barcode_dest => integer_to_binary(maps:get(Q2, QTCoordinatesToBarcodeMap)),
                                qt_coord_dest => jsx:encode(tuple_to_list(Q2))
                            }
                        }
                    end,
                    NotMatchedCoords
                ),
            {error, ErrorDetails};
        _Valid ->
            {ok, <<"Valid">>}
    end.

validate_internal(MapValues, ZoneList) ->
    {
        {GOCoordinatesToBarcodeMap, GOCoordinateOrderedList}, 
        {QTCoordinatesToBarcodeMap, QTCoordinateOrderedList}
    } = get_go_qt_coords(MapValues, ZoneList), 
    GoCount = length(GOCoordinateOrderedList),
    QTCount = length(QTCoordinateOrderedList),
    if
        GoCount == QTCount ->
            QTGOCoords = lists:zip(GOCoordinateOrderedList, QTCoordinateOrderedList),
            QTGOCoords1 = lists:droplast(QTGOCoords),
            QTGOCoords2 = tl(QTGOCoords),
            QTGOCoordsPairs = lists:zip(QTGOCoords1, QTGOCoords2),
            NotMatchedCoords =
                lists:filter(
                    fun({
                            {{GX1, GY1}, {QX1, QY1}}, 
                            {{GX2, GY2}, {QX2, QY2}}
                    }) ->
                        DGX = abs(GX2 - GX1),
                        DGY = abs(GY2 - GY1),
                        DQX = abs(QX2 - QX1),
                        DQY = abs(QY2 - QY1),
                        DGX =/= DQX orelse DGY =/= DQY
                    end,
                    QTGOCoordsPairs
                ),
            case NotMatchedCoords of
                [] ->
                    true;
                NotMatchedCoords ->
                    {false, NotMatchedCoords, GOCoordinatesToBarcodeMap, QTCoordinatesToBarcodeMap}
            end;
        true ->
            {false, coordinate_count_diff, GoCount, QTCount}
    end.