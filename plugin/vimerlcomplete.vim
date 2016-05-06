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

command! -nargs=* EcompleteGen call VimerlcompleteGen <args>

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

function! vimerlcomplete#Complete(findstart, base)
    " first invoke try to find replace column where should start
    if a:findstart
        return s:calc_return_column()
    else
        " second invoke a:base is the string should replace
        if a:base =~ '\w\+:\w*' 
            " match module:(function)
            return s:search_external(a:base)
        else
            " I don't want supply local function because other method can do 
            " I'm lazy
            return -1
        endif
    endif
endfunction

" try to return replace column  
function! s:calc_return_column() 
    let line = strpart(getline('.'), 0, col('.') - 1)
    let tmplist = split(line, '\s\|(\')
    if tmplist == []
        return -1
    endif
    " 添加'$'防止前面存在一样的string
    " '$' is used for handle pattern like "lists:   lists:(cursor here)"
    let column = match(line, tmplist[-1].'$')
    return column
endfunction

" base is like "module:(fun)"
function! s:search_external(base) 
    let sp = split(a:base, ":")
    " func maybe[] or [funname]
    let [module; func] = sp
    let filename = module .'.parse'
    if s:is_file_exist(s:parse_path, module.'.parse')
        " offical modules
        return s:search_offical_module_functions(module, func)
    else
        " user modules
        let filepath = s:get_user_module_filepath(module)
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
        echom "None"
    else
        let str1 = strpart(grepresult, 0, pos)
        let str2 = strpart(grepresult, pos, strlen(grepresult) - pos - 1)
        let export = split(str1, '\n')
        let allfun = split(str2, '\n')
        let fun2 = filter(allfun, 's:is_user_fun_export(export, v:val)')
        return map(fun2, 's:form_user_result_onlyexport(a:module, v:val)')
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

" form user function only exported
function! s:form_user_result_onlyexport(module, str)
    let name = matchstr(a:str, '\w\+(')
    return {'word':a:module.':'.name, 'abbr': a:str}
endfunction

function! s:is_file_exist(path, filename)
    return !empty(globpath(a:path, a:filename))
endfunction

function! s:is_user_fun_export(export_list, fun_str)
   let tmp = split(a:fun_str, '(\|,\|)') 
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
    setlocal omnifunc=vimerlcomplete#Complete
    inoremap <buffer> <C-J>         <ESC>:call Try_complete()<CR>
    augroup vimerlautocmd
        au!
        autocmd CompleteDone <buffer> call feedkeys("\<ESC>\<C-W>\<C-Z>a")
        autocmd  InsertCharPre <buffer> if  v:char == ':' 
                    \ | if g:vimerl_complete_auto
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
    let findcmd = "find " . currentpath . " " . "-name" . " " . a:module . ".erl"
    let find = system(findcmd)
    return find
endfunction
