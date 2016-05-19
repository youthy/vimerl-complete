" =============================================================================
" Vimerl-Complete
" Author: youthy
" Email: yuyouqi101205@gmail.com
" first generate date: 2016.05.04
" =============================================================================
if exists("b:vimerl_complete_loaded")
    finish
else
    let b:vimerl_complete_loaded = 1
endif

if !exists("g:vimerl_complete_only_export")
    let g:vimerl_complete_only_export = 1
endif

if !exists('g:vimerl_complete_auto')
    let g:vimerl_complete_auto = 1
endif

let s:exec_script = expand('<sfile>:p:h') . '/vimerlcompletegen.erl'
let s:parse_path = expand('<sfile>:p:h') . '/parsetag/'
let s:home_path = split(expand('<sfile>:p:h'), '.vim')[0]

autocmd FileType erlang call VimerlCompleteSet()

command! -nargs=? EcompleteGen call VimerlcompleteGen(<q-args>)

" gen .parse file
function! VimerlcompleteGen(version)
    if a:version !=# ""
        let doc_dir = "docs-" . a:version
        if !s:is_file_exist(s:home_path, doc_dir)
            echoerr "thereis no directory". ' '. s:home_path . doc_dir. ' ' . "please download first"
            return 0
        endif
        let scriptarg = s:home_path.doc_dir
    else
        let scriptarg = ""
    endif
    let script_output = system(s:exec_script . ' ' . scriptarg)
    if !v:shell_error
        return 0
    else
        echoerr "vimerlcompletegen " . script_output
    endif
endfunction

function! vimerlcomplete#Complete(findstart, base)
    " first invoke try to find replace column where should start
    if a:findstart
        return s:calc_return_column()
    else
        " second invoke a:base is the string should replace
        if a:base =~ '\w\+:\w*' 
            " match module:(function)
            return s:search_external(a:base, 1)
        else
            return s:search_local(a:base)
        endif
    endif
endfunction

" try to return replace column  
function! s:calc_return_column() 
    let line = strpart(getline('.'), 0, col('.') - 1)
    " 添加'$'防止前面存在一样的string
    " '$' is used for handle pattern like "lists:   lists:(cursor here)"
    let column = match(line, '\w\+:*\w*$')
    return column
endfunction

" base is like "module:(fun)"
function! s:search_external(base, prefixmodule) 
    let sp = split(a:base, ":")
    " func maybe[] or [funname]
    let [module; func] = sp
    let filename = module .'.parse'
    if s:is_file_exist(s:parse_path, module.'.parse')
        " offical modules
        return s:search_offical_module_functions(module, func, a:prefixmodule)
    else
        " user modules
        let filepath = s:get_user_module_filepath(module.'.erl')
        if empty(filepath)
            " can't find module
            return -1
        endif
        if g:vimerl_complete_only_export
            return s:search_user_module_export_functions(module, func, filepath)
        else
            return s:search_user_module_functions(module, func, filepath)
        endif
    endif
endfunction

function! s:search_local(base)
    let modresult = s:search_module(a:base)
    let erlangfun = s:search_external('erlang:'.a:base, 0)
    return modresult + erlangfun + s:search_local_fun(a:base)
endfunction

function! s:search_module(base)
    let officalpath = globpath(s:parse_path, a:base.'*'.'.parse')
    let userpath = s:get_user_module_filepath(a:base.'*.erl')
    let officalmods = split(substitute(officalpath, '[^\n]*/\(\w*\).parse', '\1', 'g'), '\n')
    let usermods = split(substitute(userpath, '[^\n]*/\(\w*\).erl', '\1', 'g'), '\n')
    return map(extend(officalmods, usermods), '{"word":v:val, "kind":"m"}')
endfunction

function! s:search_local_fun(base)
    let lnum = 1
    let return = []
    while lnum != 0 
        let tmp = matchstr(getline(lnum), '^'.a:base.'.*)')
        let lnum = nextnonblank(lnum + 1)
        if empty(tmp)
            continue
        else
            call add(return, {'word':matchstr(tmp, '^\w\+('), 'abbr':tmp, 'kind':'f', 'dup':1})
        endif
    endwhile
    return return
endfunction

function! s:search_offical_module_functions(module, func, prefixmodule)
    if a:func == []
        let arg = '.*'
    else
        let arg = '^' . a:func[0]
    endif
    if a:prefixmodule
        let prefix = a:module.':'
    else
        let prefix = ""
    endif
    let grepcmd = 'grep ' . '"'. arg . '"' . ' ' . s:parse_path . a:module . '.parse'
    let result = map(split(system(grepcmd), '\n'), 's:form_offical_result(prefix, v:val)')
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

function! s:search_user_module_export_functions(module, func, filepath)
    if a:func == []
        let arg = '\w+/\d+|^\w+\(.*\)'
    else
        let arg = a:func[0].'\w*/\d+' . '|^' . a:func[0]. '.*\(.*\)'
    endif
    let grepcmd = 'grep -P -o ' . '"' . arg . '"' . ' ' .  a:filepath
    let grepresult = system(grepcmd)
    let pos = matchend(grepresult, '\(\w\+/\d\+\n\)*')
    if pos < 1
        return []
    else
        let str1 = strpart(grepresult, 0, pos)
        let str2 = strpart(grepresult, pos, strlen(grepresult) - pos - 1)
        let export = split(str1, '\n')
        let allfun = map(split(str2, '\n'), 'matchstr(v:val, ".\\{-})")')
        let fun2 = filter(allfun, 's:is_user_fun_export(export, v:val)')
        return map(fun2, 's:form_user_result_onlyexport(a:module, v:val)')
    endif
endfunction

" form offical each result
function! s:form_offical_result(prefix, str)
    let [word, replace; type] = split(a:str, '@')
    if empty(type)
        return {'word':a:prefix . word . '(', 'abbr':replace, 'kind':'f', 'dup':1}
    else
        return {'word':a:prefix . word . '(', 'abbr':replace, 'info':type[0], 'kind':'f', 'dup':1}
endfunction

" form user function result
function! s:form_user_result(module, str)
    let name = matchstr(a:str, '\w\+(')
    return {'word':a:module.':'.name, 'abbr': matchstr(a:str, '.*)'), 'kind':'f', 'dup':1}
endfunction

" form user function only exported
function! s:form_user_result_onlyexport(module, str)
    let name = matchstr(a:str, '\w\+(')
    return {'word':a:module.':'.name, 'abbr': a:str, 'dup': 1}
endfunction

function! s:is_file_exist(path, filename)
    return !empty(globpath(a:path, a:filename))
endfunction

function! s:is_user_fun_export(export_list, fun_str)
    " change #record{}, tuple{}, and list[] to a simple Text
    let str2 = substitute(a:fun_str, '{.\{-}}\|\[.*]', 'Text', 'g')
    let tmp = split(str2, '(\|,\|)') 
    if tmp[-1] == ''
        let num = 0
    else
        let num = len(tmp) - 1
    endif
    if index(a:export_list, tmp[0].'/'.num) != -1
        return 1
    else
        return 0
    endif
endfunction

function! vimerlcomplete#Tab() 
    if pumvisible()
        return "\<C-N>"
    endif
    if s:get_line_cursor() =~ '\w\+:*$' 
        return "\<C-X>\<C-O>"
    else
        return "\<Tab>"
    endif
endfunction

function! VimerlCompleteSet()
    setlocal omnifunc=vimerlcomplete#Complete
    setlocal completeopt=menuone,preview,longest
    inoremap <buffer> <TAB>  <C-R>=vimerlcomplete#Tab()<CR>
    augroup vimerlautocmd
        au!
        autocmd InsertLeave <buffer> if s:PreviewWindowOpened()|pclose|endif
        autocmd InsertCharPre <buffer> if  v:char == ':' 
                    \ | if g:vimerl_complete_auto && (s:get_line_cursor() =~ '\w\+$')
                        \ | call feedkeys("\<C-X>\<C-O>") 
                        \ | endif
                    \ | endif
    augroup END
endfunction

function! s:get_user_module_filepath(module)
    let currentpath = split(expand('%:p:h'), 'src')[0]
    if empty(currentpath)
        return ""
    endif
    let find = globpath(currentpath, '**/'.a:module)
    return find
endfunction

function! s:PreviewWindowOpened()
    for nr in range(1, winnr('$'))
        if getwinvar(nr, "&pvw") == 1
            return 1
        endif  
    endfor
    return 0
endfunction

function! s:get_line_cursor()
    return strpart(getline('.'), 0, col('.') - 1)
endfunction
