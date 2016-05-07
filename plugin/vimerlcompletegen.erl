#!/usr/bin/env escript

-mode(compile).
-define(RE_FUNCTION, "<h3 id=\"(?<name>\\w+)\/\\d+\">(?<detail>.*)</h3>\\s+(?<type><ul.*/ul>\|\\s+)").

main([Path]) ->
    Files = filelib:wildcard(Path ++"/" ++ "*/*.html"),
    lists:map(fun parse_file/1, Files),
    io:format("~p~n", [Files]).

parse_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Data = parse_bin(Bin),
    BaseName = filename:basename(FileName),
    [Module, _] = string:tokens(BaseName, "."),
    ok = file:write_file(get_output_file(Module), Data).

parse_bin(Bin) ->
    case re:run(Bin, ?RE_FUNCTION, [global, {capture, [name, detail, type], binary}]) of
        nomatch -> [];
        {match, List} -> 
            [parse_part(Match) || Match <- List]
    end.

parse_part([FunName, FunDetail, Type]) ->
    Detail2 = parse_detail(FunDetail),
    Type2 = parse_type(Type),
    case Type2 of
        <<>> -> list_to_binary([FunName, "@", Detail2, "\n"]);
        _ -> list_to_binary([FunName, "@", Detail2, "@", Type2, "\n"])
    end.

get_output_dir() ->
    PluginDir = filename:dirname(filename:absname(escript:script_name())),
    Name = PluginDir ++ "/parsetag/",
    ok = filelib:ensure_dir(Name),
    Name.

get_output_file(Module) when is_list(Module) ->
    get_output_dir() ++ "/" ++ Module ++ ".parse".

parse_detail(Detail) ->
    Detail2 = re:replace(Detail, "\s<a.*\">|<\/a>", " ", [global, {return, binary}]),
    replace_gt(Detail2).

parse_type(<<>>) ->
    <<>>;
parse_type(<<"\n">>) ->
    <<>>;
parse_type(Type) ->
    Type2 = re:replace(Type, "<.*<code>|<a.*\">|<\/\[a-z]+>", " ", [global, {return, binary}, ungreedy]),
    replace_gt(Type2).

replace_gt(Bin) ->
    re:replace(Bin, "&gt;", ">", [global, {return, binary}]).
