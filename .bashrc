#!/bin/bash

synclient VertScrollDelta=-114
synclient TapButton3=2
synclient VertEdgeScroll=0

[[ "$(command -v __git_ps1)" ]] && {
  #remove trailing "\$" (and trailing space) from $PS1 before adding call to __git_ps1
  PS1=${PS1:0:-3}
  PS1="$PS1\$(__git_ps1 ':\[\033[01;93m\]%s\[\033[00m\]')\$ "
  export PS1
}
