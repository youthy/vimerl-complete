#!/usr/bin/env escript

-mode(compile).
-include_lib("xmerl/include/xmerl.hrl").

-define(RE_FUNCTION, "<h3 id=\"(?<name>\\w+)\/\\d+\">(?<detail>.*)</h3>\\s+(?<type><ul.*/ul>\|\\s+)").
-define(A2S(Atom), atom_to_list(Atom)).
-define(S2A(List), list_to_atom(List)).
-define(N2S(Number), integer_to_list(Number)).

main(Argc) ->
    delete_previous_file(),
    main0(Argc).
main0([]) ->
    ErlFiles = find_local_src_path(),
    parse_erl_files(ErlFiles);
main0([Path]) ->
    Files = filelib:wildcard(Path ++"/" ++ "*/*.html"),
    {ok, Mp} = re:compile(?RE_FUNCTION),
    [parse_file(File, Mp) || File <- Files].
%% =============================================================================
%% Parse .erl files
%% =============================================================================
parse_erl_files([]) ->
    halt(-1);
parse_erl_files(List) when is_list(List) ->
    lists:map(fun parse_erl/1, List).

parse_erl(FileName) ->
    Module = get_module_name(FileName),
    try  
        {_, Doc} = edoc:get_doc(FileName),
        Data = parse_edoc(Doc),
        ok = file:write_file(get_output_file(Module), list_to_binary(Data))
    catch 
        _Type:_Error -> 
            Data2 = parse_module_info(list_to_atom(Module)),
            ok = file:write_file(get_output_file(Module), list_to_binary(Data2))
    end.

find_local_src_path() ->
    LibDir = code:lib_dir(),
    filelib:wildcard(LibDir ++ "/" ++ "*/src/*.erl").

%% XML Element
%% -record(xmlElement,{
%%         name,         % atom()
%%         expanded_name = [],   % string() | {URI,Local} | {"xmlns",Local}
%%         nsinfo = [],          % {Prefix, Local} | []
%%         namespace=#xmlNamespace{},
%%         parents = [],     % [{atom(),integer()}]
%%         pos,          % integer()
%%         attributes = [],  % [#xmlAttribute()]
%%         content = [],
%%         language = "",    % string()
%%         xmlbase="",           % string() XML Base path, for relative URI:s
%%         elementdef=undeclared % atom(), one of 0red | prolog | external | element]
%%     }).
parse_edoc(Doc) ->
    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    parse_fun(Funs).

parse_fun(Funs) when is_list(Funs) ->
    parse_fun(Funs, []).
parse_fun([], Acc) ->
    Acc;
parse_fun([Fun|T], Acc) ->
    Name = get_xml_attribute(Fun, "name"),
    Args0 = xmerl_xpath:string("typespec/type/fun/argtypes/type", Fun),
    Args = lists:map(fun(Arg) -> get_xml_attribute(Arg, "name") end, Args0),
    Argstr = make_str(Args),
    Return = analyze_return(Fun),
    Result = [Name ,"@" ,Name ,Argstr ," -> " ,Return ,"\n"],
    parse_fun(T, [Result|Acc]).

parse_module_info(Mod) ->
    parse_module_info(Mod:module_info(exports), []).
parse_module_info([], Acc) ->
    Acc;
parse_module_info([{Name, Arity}|T], Acc) ->
    Item = [?A2S(Name) ,"@" ,?A2S(Name) ,"/" ,?N2S(Arity) ,"\n"],
    parse_module_info(T, [Item|Acc]).
%% =============================================================================
%% The functions below copied from vim-erlang-omnicomplete
%% Originally I only parse htmls
%% https://github.com/vim-erlang/vim-erlang-omnicomplete
analyze_return(Fun) ->
    case xmerl_xpath:string("typespec/type/fun/type/*", Fun) of
        [Return] ->
            simplify_return(xmerl_lib:simplify_element(Return));
        [] ->
            throw(no_spec)
    end.

simplify_return({typevar, [{name, Name}], _}) ->
    Name;
simplify_return({type, _, [Type]}) ->
    simplify_return(Type);
simplify_return({abstype, _, [Type|_]}) ->
    {erlangName, Attrs, _} = Type,
    Name = proplists:get_value(name, Attrs),
    Name ++ "()";
simplify_return({record, _, [Type]}) ->
    simplify_return(Type) ++ "()";
simplify_return({nonempty_list, _, [Type]}) ->
    "[" ++ simplify_return(Type) ++ "]";
simplify_return({tuple, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "{" ++ string:join(Elems, ", ") ++ "}";
simplify_return({list, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "[" ++ string:join(Elems, ", ") ++ "]";
simplify_return({paren, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "(" ++ string:join(Elems, ", ") ++ ")";
simplify_return({union, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    string:join(Elems, " | ");
simplify_return({integer, [{value, Val}], _}) ->
    Val;
simplify_return({atom, [{value, Val}], _}) ->
    Val;
simplify_return({nil, _, _}) ->
    "[]";
simplify_return({map_field, _, [Key, Value]}) ->
    simplify_return(Key) ++ " => " ++ simplify_return(Value);
simplify_return({map, _, PairList}) ->
    Pairs = string:join([ simplify_return(Pair) || Pair <- PairList ], ", "),
    "#{" ++ Pairs ++ "}".
%% =============================================================================
make_str([]) ->
    "()";
make_str(List) when is_list(List) ->
    make_str("(", List).
make_str(Acc, []) ->
    Acc;
make_str(Acc, [H]) ->
    make_str(Acc ++ H ++ ")", []);
make_str(Acc, [H|T]) ->
    make_str(Acc ++ H ++ ", ", T).

%% =============================================================================
%% Parse .html files
%% =============================================================================
parse_file(FileName, Mp) ->
    {ok, Bin} = file:read_file(FileName),
    Data = parse_bin(Bin, Mp),
    Module = get_module_name(FileName),
    ok = file:write_file(get_output_file(Module), Data).

delete_previous_file() ->
    Files = filelib:wildcard(get_output_dir() ++ "*.parse"),
    lists:map(fun(F) -> file:delete(F) end, Files).

parse_bin(Bin, Mp) ->
    case re:run(Bin, Mp, [global, {capture, [name, detail, type], binary}]) of
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
parse_type(<<" ">>) ->
    <<>>;
parse_type(Type) ->
    Type2 = re:replace(Type, "<.*<code>|<a.*\">|<\/\[a-z]+>", " ", [global, {return, binary}, ungreedy]),
    replace_gt(Type2).

replace_gt(Bin) ->
    re:replace(Bin, "&gt;", ">", [global, {return, binary}]).

%% =============================================================================
%% Internal 
%% =============================================================================
get_module_name(FileName) ->
    BaseName = filename:basename(FileName),
    [Module, _] = string:tokens(BaseName, "."),
    Module.

get_xml_attribute(Xml, Name) ->
    [Attr] = xmerl_xpath:string("@" ++ Name, Xml),
    Attr#xmlAttribute.value.
