export BASH_SILENCE_DEPRECATION_WARNING=1

# history size
export HISTFILESIZE=1000000
export HISTSIZE=1000000

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_OPTS='--layout=reverse'
export FZF_DEFAULT_COMMAND='rg --files --follow --no-ignore-vcs --hidden -g "!{stubs/*,vendor/*,node_modules/*,.git/*}"'

export PS1='\[\033[0;33m\][\w]\[\033[0m\]\[\033[0;31m\]$(if git rev-parse --git-dir > /dev/null 2>&1; then echo " - ("; fi)$(git branch 2>/dev/null | grep "^*" | colrm 1 2)\[\033[0;31m\]$(if git rev-parse --git-dir > /dev/null 2>&1; then echo ")"; fi)\[\033[0m\033[0;32m\] \$\[\033[0m\033[0;32m\]\[\033[0m\] '

export EDITOR=vim

export PATH=$PATH:$HOME/go/bin

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
