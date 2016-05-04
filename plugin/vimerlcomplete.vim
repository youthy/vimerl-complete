if exists("b:vimerl_complete_loaded")
    finish
else
    let b:vimerl_complete_loaded = 1
endif

let s:exec_script = expand('<sfile>:p:h') . '/vimerlcompletegen.erl'
let s:parse_path = expand('<sfile>:p:h') . '/parsetag/'
let s:home_path = split(expand('<sfile>:p:h'), '.vim')[0]

autocmd FileType erlang call VimerlCompleteSet()
autocmd! CompleteDone * call feedkeys("\<ESC>\<C-W>\<C-Z>")

command! -nargs=* VcompleteGen call VimerlcompleteGen <args>

function! vimerlcomplete#Complete(findstart, base)
    if a:findstart
        return s:calc_return_column()
    else
        echom a:base
        if a:base =~ '\w\+:\w*'
            echom 'external'
            return s:search_external(a:base)
        else
            echom 'local'
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
    echom column
    return column
endfunction

function! s:search_external(base) 
    echom a:base
    let sp = split(a:base, ":")
    " func可能为[], 或者[funname]
    let [module; func] = sp
    if func == []
        let arg = '.* '
    else
        let arg = '^' . func[0]
    endif
    let filename = module .'.parse'
    if !s:is_file_exist(s:parse_path, filename)
        return -1
    endif
    let grepcmd = 'grep ' . '"'. arg . '"' . ' ' . s:parse_path . filename 
    let result = map(split(system(grepcmd), '\n'), 's:form_result(module, v:val)')
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

" form each result
function! s:form_result(module, str)
    let [word, replace; type] = split(a:str, '@')
    echom word replace
    if empty(type)
        return {'word':a:module . ':'. word . '(', 'abbr':replace}
    else
        return {'word':a:module . ':'. word . '(', 'abbr':replace, 'info':type[0]}
endfunction

function! s:is_file_exist(path, filename)
    return !empty(globpath(a:path, a:filename))
endfunction

function! Try_complete() 
    call feedkeys('a')
    let line = strpart(getline('.'), 0, col('.'))
    echom line
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

