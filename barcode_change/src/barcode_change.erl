-module(barcode_change).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Path]) ->
    io:format("Path: ~p~n", [Path]),
    {ok, F} = read_json(Path ++ "/gmc_map.json"),
    FloorList = jsx:decode(erlang:list_to_binary(F)),
    UpdatedFloorList =
        lists:map(
            fun(FloorDetails) ->
                MapValues = proplists:get_value(<<"map_values">>, FloorDetails, []),
                NewMapValues = apply_barcode_change(MapValues, 'X*512+Y'),
                lists:keyreplace(<<"map_values">>, 1, FloorDetails, {<<"map_values">>, NewMapValues})
            end,
            FloorList
        ),
    NewJson = jsx:prettify(jsx:encode(UpdatedFloorList, [{indent, 2}, {space, 1}])),
    %DataToWrite = io_lib:format("~tp\n", [NewJson]),
    file:write_file(Path ++ "/gmc_updated_map.json", NewJson, []),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
read_json(Path) ->
    {ok, File} = file:open(Path,[read]),
    file:read(File,1024 * 1024).

apply_barcode_change(MapValues, 'X*512+Y') ->
    WorldCoordinateList =
        lists:map(
            fun(NodeDetails) ->
                JsonWorldCoordinate = 
                    proplists:get_value(<<"world_coordinate">>, NodeDetails, []),
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
            fun(NodeDetails) ->
                JsonWorldCoordinate = 
                    proplists:get_value(<<"world_coordinate">>, NodeDetails, []),
                [WX, WY] = jsx:decode(JsonWorldCoordinate),
                {QTX, QTY} = maps:get({WX, WY}, WCToQTCoordMap),
                QTBarcode = erlang:integer_to_binary(512*QTX + QTY),
                lists:keyreplace(<<"barcode">>, 1, NodeDetails, {<<"barcode">>, QTBarcode})
            end,
            MapValues
        ), 
    NewMapValues.