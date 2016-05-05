if exists("b:vimerl_complete_loaded")
    finish
else
    let b:vimerl_complete_loaded = 1
endif

let s:exec_script = expand('<sfile>:p:h') . '/vimerlcompletegen.erl'
let s:parse_path = expand('<sfile>:p:h') . '/parsetag/'
let s:home_path = split(expand('<sfile>:p:h'), '.vim')[0]

autocmd FileType erlang call VimerlCompleteSet()
autocmd! CompleteDone * call feedkeys("\<ESC>\<C-W>\<C-Z>a")

command! -nargs=* EcompleteGen call VimerlcompleteGen <args>

function! vimerlcomplete#Complete(findstart, base)
    if a:findstart
        return s:calc_return_column()
    else
        if a:base =~ '\w\+:\w*' 
            return s:search_external(a:base)
        else
            return -1
        endif
    endif
endfunction

function! s:calc_return_column() 
    let line = strpart(getline('.'), 0, col('.') - 1)
    let tmplist = split(line, '\s\|(\')
    if tmplist == []
        return -1
    endif
    " 添加'$'防止前面存在一样的string
    let column = match(line, tmplist[-1].'$')
    return column
endfunction

function! s:search_external(base) 
    let sp = split(a:base, ":")
    " func可能为[], 或者[funname]
    let [module; func] = sp
    let filename = module .'.parse'
    if s:is_file_exist(s:parse_path, module.'.parse')
        " offical modules
        return s:search_offical_module_functions(module, func)
    else
        " user modules
        let filepath = Get_user_module_filepath(module)
        if empty(filepath)
            " can't find module
            return -1
        endif
        return s:search_user_module_functions(module, func, filepath)
    endif
    endfunction

function! s:search_offical_module_functions(module, func)
    if a:func == []
        let arg = '.* '
    else
        let arg = '^' . a:func[0]
    endif
    let grepcmd = 'grep ' . '"'. arg . '"' . ' ' . s:parse_path . a:module . '.parse'
    let result = map(split(system(grepcmd), '\n'), 's:form_offical_result(a:module, v:val)')
    return result
endfunction

function! s:search_user_module_functions(module, func, filepath)
    if a:func == []
        let arg = '^\w\+('
    else
        let arg = '^' . a:func[0]
    endif
    let grepcmd = 'grep ' . '"'. arg . '"' . ' ' . a:filepath 
    let result = map(split(system(grepcmd), '\n'), 's:form_user_result(a:module, v:val)')
    return result
endfunction

" gen .parse file
function! VimerlcompleteGen(version)
    if type(a:version) != type("")
        echoerr "badarg: version must be string"
        return 0
    endif
    let doc_dir = "docs-" . a:version
    if !s:is_file_exist(s:home_path, doc_dir)
        echoerr "thereis no directory". ' '. s:home_path . doc_dir. ' ' . "please download first"
        return 0
    endif
    let script_output = system(s:exec_script . ' ' . s:home_path . doc_dir)
    if !v:shell_error
        return 0
    else
        echoerr "vimerlcompletegen " . script_output
    endif
endfunction

" form offical each result
function! s:form_offical_result(module, str)
    let [word, replace; type] = split(a:str, '@')
    if empty(type)
        return {'word':a:module . ':'. word . '(', 'abbr':replace}
    else
        return {'word':a:module . ':'. word . '(', 'abbr':replace, 'info':type[0]}
endfunction

" form user function result
function! s:form_user_result(module, str)
    let name = matchstr(a:str, '\w\+(')
    return {'word':a:module.':'.name, 'abbr': matchstr(a:str, '.*)')}
endfunction

function! s:is_file_exist(path, filename)
    return !empty(globpath(a:path, a:filename))
endfunction

function! Try_complete() 
    call feedkeys('a')
    let line = strpart(getline('.'), 0, col('.'))
    if line =~ '.*:\w*$'
        call feedkeys("\<C-X>\<C-O>", 'n')
    else
        call feedkeys("\<c-x>\<c-n>", 'n')
    endif
endfunction

function! VimerlCompleteSet()
    set omnifunc=vimerlcomplete#Complete
    inoremap <buffer> <C-J>         <ESC>:call Try_complete()<CR>
endfunction

function! Get_user_module_filepath(module)
    let currentpath = split(expand('%:p:h'), 'src')[0]
    if empty(currentpath)
        return ""
    endif
    let findcmd = "find " . currentpath . " " . "-name" . " " . a:module . ".erl"
    let find = system(findcmd)
    return find
endfunction
