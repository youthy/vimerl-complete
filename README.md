# vimerl-complete
Vim plugin for erlang code auto-completation

## Install
1. If you use Vundle, then edit .vimrc in your $HOME path, add 
```
Plugin 'youthy/vimerl-complete'
```` 

after other plugins. Then type
```
:PluginInstall
```` 

in vim to install.
2. or just download and put extract directory `vimerl-complete` into your plugin directory.

## Usage
1. This plugin parse offical docs on [erldocs](http://erldocs.com/). please download a version which you need and extract into your
$HOME directory. like `/home/youthy/docs-18.3`.

2. Then run 
```
:EcompleteGen("18.3")
```

in vim to generate tags for completation. ("18.3" is the version I used. It can be like "R15B01" etc.)

By default, It will automaticlly display module functions after you type `module:`.

And the functions only include exported functions. 

![](http://i1156.photobucket.com/albums/p578/crossshura/optimized_zpsmwhao7bh.gif)

You can change default settings by add
```
" set this value to 0 to disable the auto completation
let g:vimerl_complete_auto = 0
" set this value to 0 to enable display all module functions include not exported
let g:vimerl_complete_only_export = 0
```

into your `.vimrc` file to change the default settings.
If you disable auto-display, then you can Use `<C-J>` which means `CTRL + J` to list completation. 
If the words before cursor do not have `:` it displays what `<C-X><C-N>` does.


If the module is not in the offical list, it will search `.erl` files in the `src` directory's parent directory recursively.If no 
file match the module name.It will print error report.
