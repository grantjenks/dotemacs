# Bash RC
# Symlinks:
# $HOME/.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias dir='ls -Ahl'
alias del='rm'
alias quit='exit'
alias ll='ls -hAltr'
alias ccat='pygmentize -f terminal256 -O style=native'

PS1="\D{%F %T} \u @ \h : \w \n$ "

# Eternal bash history.
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

if [[ $- == *i* ]]
then
    bind '"\e[A": history-search-backward'
    bind '"\eOA": history-search-backward'
    bind '"\e[B": history-search-forward'
    bind '"\eOB": history-search-forward'
fi

export PATH="$PATH:$HOME/bin:$HOME/.local/bin"
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

if [ -f "$HOME/repos/openai/personal/grantjenks/bashrc" ]; then
    source "$HOME/repos/openai/personal/grantjenks/bashrc"
fi

eval "$(am shell)"
eval $(python -m rcall.shell)
eval "$(bbb complete init bash)"
source <(/Users/grantjenks/.openai/bin/brix completion bash)
export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

function update_nvm() (
    export GIT_DIR_PARAM="--git-dir=$NVM_DIR/.git"
    export GIT_WORKTREE_PARAM="--work-tree=$NVM_DIR"
    git $GIT_DIR_PARAM $GIT_WORKTREE_PARAM checkout $(git $GIT_DIR_PARAM $GIT_WORKTREE_PARAM describe --abbrev=0 --tags --match "v[0-9]*" $(git $GIT_DIR_PARAM $GIT_WORKTREE_PARAM rev-list --tags --max-count=1))
)
export API_REPO_PATH="/Users/grantjenks/repos/openai/api"
source ~/.api_shell_include
source /Users/grantjenks/repos/openai/api/applied-devtools/completions/applied_completions.bash

eval "$(z profile)"
