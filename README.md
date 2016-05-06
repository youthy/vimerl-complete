# vimerl-complete
Vim plugin for erlang code auto-completation

## Install
1. If you use Vundle, then edit .vimrc in your $HOME path, add `Plugin 'youthy/vimerl-complete'` after other plugins. Then type
`:PluginInstall` in vim to install.
2. or just download and put extract directory `vimerl-complete` into your plugin directory.

## Usage
This plugin parse offical docs on [erldocs](http://erldocs.com/). please download a version which you need and extract into your
$HOME directory. like `/home/youthy/docs-18.3`.

Then run 
```
:EcompleteGen("18.3")
```

in vim to generate tags for completation. ("18.3" is the version I used. It can be like "R15B01" etc.)

Use `<C-J>` which means `CTRL + J` to list completation. If the words before cursor do not have `:` it displays what `<C-X><C-N>` 
does.

You can add
```
let g:vimerl_complete_auto = 1
```
to your `.vimrc` file to make the completation automaticlly without press `<C-J>` after you type `:`. In contrast, just remove it 
or set the value to `0` to close auto-completation.

![](http://i1156.photobucket.com/albums/p578/crossshura/optimized_zpsmwhao7bh.gif)

If the module is not in the offical list, it will search `.erl` files in the `src` directory's parent directory recursively.If no 
file match the module name.It will print error report.
