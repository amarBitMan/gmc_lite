-module(gmc_lite_file_utils).
-export([
    read_json/1,
    write_files/2
]).

read_json(Path) ->
    {ok, File} = file:open(Path, [read]),
    {ok, Json} = file:read(File, 100 * 1024 * 1024),
    jsx:decode(erlang:list_to_binary(Json), [return_maps, {labels, atom}]).

write_files(FileInfoList, FilePath) ->
    case file:list_dir(FilePath) of
        {ok, _Files} ->
            ok;
        _ ->
            ok = file:make_dir(FilePath)
    end,
    NewFiles =
        lists:map(
            fun({FileName, FileData}) ->
                DataToWrite = jsx:encode(FileData),
                CompleteFilePath = FilePath ++ "/" ++ FileName,
                ok = file:write_file(CompleteFilePath, DataToWrite, []),
                {list_to_atom(FileName), list_to_binary(CompleteFilePath)}
            end,
            FileInfoList
        ),
    #{new_files => NewFiles}.
