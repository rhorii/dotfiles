#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias la='ls -alh'
alias ll='ls -lh'
if [ "$(uname)" == 'Darwin' ]; then
  alias ls='ls -G'
else
  alias ls='ls --color=auto'
  alias open='xdg-open'
fi

# prompt
if [[ -f /usr/share/git/completion/git-prompt.sh ]]; then
  . /usr/share/git/completion/git-prompt.sh
elif [[ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]]; then
  . /usr/local/etc/bash_completion.d/git-prompt.sh
fi
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=
GIT_PS1_SHOWUPSTREAM='auto'
PS1='\n\u@\h:\[\e[32m\]\]\w\[\e[0m\]$(__git_ps1 " (%s)")\n$ '

# history
HISTSIZE=32768
HISTFILESIZE="${HISTSIZE}"
HISTCONTROL='ignoreboth'

# bash-completion
if [[ -r /usr/local/etc/profile.d/bash_completion.sh  ]]; then
  BASH_COMPLETION_COMPAT_DIR='/usr/local/etc/bash_completion.d'
  . /usr/local/etc/profile.d/bash_completion.sh
fi

# fzf
if [[ -d /usr/share/fzf ]]; then
  . /usr/share/fzf/key-bindings.bash
  . /usr/share/fzf/completion.bash
elif [[ -d /usr/local/opt/fzf/shell ]]; then
  . /usr/local/opt/fzf/shell/key-bindings.bash
  . /usr/local/opt/fzf/shell/completion.bash
fi

cr() {
  local repo
  repo=$(ghq list | fzf) && cd $(ghq root)/${repo}
}
