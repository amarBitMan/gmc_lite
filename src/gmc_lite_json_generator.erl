-module(gmc_lite_json_generator).
-export([
    generate_rack_json/1
]).

generate_rack_json(#{file_dir := FilePath}) ->
    case file:list_dir(FilePath) of
        {ok, []} ->
            {error, <<"Folder Empty">>};
        {ok, JsonFiles} ->
            case lists:member("racks.csv", JsonFiles) of
                false ->
                    {error, <<"racks.csv file not found">>};
                true ->
                    RackData = 
                        gmc_lite_file_utils:parse_csv_file(FilePath ++ "/racks.csv"),
                    RackJsonableData = generate_rack_json(RackData),
                    Response = 
                        gmc_lite_file_utils:write_files([{"racks.json", RackJsonableData}], FilePath),
                    {ok, Response}
            end
    end;
generate_rack_json(RacksData) when is_list(RacksData) ->
    [
        #{
            direction => theta_to_direction(RackTheta),
            id => RackId,
            is_stored => true,
            last_store_position => RackBarcode,
            lifted_butler_id => null,
            position => RackBarcode,
            racktype => RackType,
            reserved_store_position => <<"undefined">>
        } || [RackId, RackBarcode, RackTheta, RackType] <- RacksData
    ].
  
theta_to_direction(<<"0">>) -> 1;
theta_to_direction(<<"90">>) -> 0;
theta_to_direction(<<"180">>) -> 3;
theta_to_direction(<<"-180">>) -> 3;
theta_to_direction(<<"-90">>) -> 2.
