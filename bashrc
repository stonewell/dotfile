alias e='emacsclient -n'
alias et='emacsclient -nw'
alias ls='ls --color=auto'

eval "$(fzf --bash)"
eval "$(starship init bash)"

export EDITOR=emacsclient
