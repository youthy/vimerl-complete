# vimerl-complete
Vim plugin for erlang code auto-completation

![](http://i1156.photobucket.com/albums/p578/crossshura/optimized2_zpszcg8yvwx.gif)
## Require
1. need [Vundle](https://github.com/VundleVim/Vundle.vim) or other Plugin Manager
2. need [Syntastic](https://github.com/scrooloose/syntastic) or some other syntax
plugin to define `erlang` filetype

## Install
Then edit .vimrc in your $HOME path, add 
```
Plugin 'youthy/vimerl-complete'
```` 

after other plugins. Then type
```
:PluginInstall
```` 

in vim to install.


## Usage
1. This plugin parse offical docs on [erldocs](http://erldocs.com/). please download a version which you need and extract into your
$HOME directory. like `/home/youthy/docs-18.3`.

2. Then run 
```
:EcompleteGen("18.3")
```

in vim to generate tags for completation. Yon need run this command **only once** unless you change your erlang version
("18.3" is the version I used. It can be like "R15B01" etc.)
3. If you don't want to download erldocs, You can also type
```
:EcompleteGen("")
```

to gen tags from erlang source files(It takes maybe a minute). You must ensure you have `.erl` src files. To find them in `code:lib_dir() ++ "*/src"` directory.
I recommand you use [kerl](https://github.com/kerl/kerl)
And this way to gen tags is copied from [vim-erlang-omnicomplete](https://github.com/vim-erlang/vim-erlang-omnicomplete)
(I just know there are this plugin, otherwise I won't write this plugin =_=).


By default, It will automaticlly display module functions after you type `module:`.

And the functions **only include exported** functions. You can change default settings by add
```
" set this value to 0 to disable the auto completation
let g:vimerl_complete_auto = 0
" set this value to 0 to enable display all module functions include not exported
let g:vimerl_complete_only_export = 0
```

into your `.vimrc` file to change the default settings.
If you **disable auto-display**, then you can Use 
```
<Tab>
``` 

to list completation. 

## Example
1. `module:` 
when you type a module name and a`:` it will displays functions exported or not in the module 
depends your settings. Module can be offical module like `lists`, `ets` or user module whose `.erl` file in the 
`src/` parent path, recursively.
like`/home/youthy/erlang_project/src`, it will match the `module.erl` in `/home/youthy/erlang_project` recursively.

2. `module:funname`
same as above,but display matches only match funname.

3. `singleword`
when there no `:` before cursor, it will display modules which match the `singleword`, and functions in `erlang` module
which match, and localfunctions.

**up_to_now, this plugin conflicts with YouCompleteMe, becase YouCompleteMe force use `completeopt-=longest`
and under this situation, Plugin always select the first matches in display menu.It annoys. But I haven't find
a beautiful solution**

