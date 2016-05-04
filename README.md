# vimerl-complete
vim plugin for erlang code auto-completation

## Install
1. if you use Vundle, edit .vimrc in your $HOME path, add Plugin `'youthy/vimerl-complete'` after other plugins. then type
`:PluginInstall` in vim.
2. or just download and put extract directory `vimerl-complete` into your plugin directory.

## Usage
this plugin parse offical docs on [erldocs](http://erldocs.com/). please download a version which you need and extract into your
$HOME directory. like `/home/youthy/docs-18.3`.

then run 
```
:VcompleteGen("18.3")
```

in vim to generate tags for completation. ("18.3" is the version I used. It can be like "R15B01" etc.)

use `<C-J>` which means `CTRL + J` to list completation. If the words before cursor do not have `:` it displays what `<C-X><C-N>` 
does.

This version can only display functions in erldocs. user defined functions maybe supplied later on.
