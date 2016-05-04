#!/usr/bin/env escript

-mode(compile).

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
    case re:run(Bin, "<h3 id=\"(?<name>\\w+)\/\\d+\">(?<detail>.*)</h3>", [global, {capture, [name, detail], binary}]) of
        nomatch -> [];
        {match, List} -> 
            [parse_part(Match) || Match <- List]
    end.

parse_part([FunName, FunDetail]) ->
    FunDetail2 = re:replace(FunDetail, "\s<a.*\">|<\/a>", " ", [global, {return, binary}]),
    FunDetail3 = re:replace(FunDetail2, "&gt;", ">", [global, {return, binary}]),
    list_to_binary([FunName, "@", FunDetail3, "\n"]).

get_output_dir() ->
    PluginDir = filename:dirname(filename:absname(escript:script_name())),
    Name = PluginDir ++ "/parsetag/",
    ok = filelib:ensure_dir(Name),
    Name.

get_output_file(Module) when is_list(Module) ->
    get_output_dir() ++ "/" ++ Module ++ ".parse".



