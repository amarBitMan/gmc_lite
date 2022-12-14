-module(gmc_lite_file_utils).
-export([
    get_data_dir/0,
    read_json/1,
    parse_csv_file/1,
    write_files/2
]).

-spec get_data_dir() -> file:filename().
get_data_dir() ->
    DataRootConfig = application:get_env(gmc_lite, data_root, cwd),
    DataRoot =
        case DataRootConfig of
            root ->
                code:root_dir();
            cwd ->
                {ok, CWD} = file:get_cwd(),
                CWD;
            priv ->
                code:priv_dir(gmc_lite)
        end,
    filename:join(DataRoot, "data").

read_json(Path) ->
    {ok, File} = file:open(Path, [read]),
    {ok, Json} = file:read(File, 100 * 1024 * 1024),
    jsx:decode(erlang:list_to_binary(Json), [return_maps, {labels, atom}]).

parse_csv_file(File) ->
    {ok, Data} = file:read_file(File),
    parse_csv_data(Data).

parse_csv_data(Data) ->
    Lines = re:split(Data, "\r|\n|\r\n", [] ), 
    [ 
        [
            begin
                case  re:split(Token, "\"", [] ) of 
                    [_,T,_] -> T;
                    [T] -> T; % if token is not surrounded by ""
                    [] -> <<"">>
                end
            end || Token <- re:split(Line, ",", [] ) 
        ] || Line <- Lines, Line =/= <<"">>
    ].

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
