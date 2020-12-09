#!/bin/bash

[[ "$(command -v __git_ps1)" ]] && {
  #remove trailing "\$" (and trailing space) from $PS1 before adding call to __git_ps1
  PS1=${PS1:0:-3}
  PS1="$PS1\$(__git_ps1 ':\[\033[01;93m\]%s\[\033[00m\]')\$ "
  export PS1
}

alias please='sudo "$BASH" -c "$(history -p !!)"'
alias nodemodules='find . -name "node_modules" | xargs rm -rf'
alias scripts='cat package.json | jq .scripts'
