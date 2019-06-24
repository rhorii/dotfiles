#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

export HISTSIZE='32768'
export HISTFILESIZE="${HISTSIZE}"
export HISTCONTROL='ignoreboth'

if [[ -f /usr/share/git/completion/git-prompt.sh ]]; then
  . /usr/share/git/completion/git-prompt.sh
fi
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM='auto'
PS1='\n\u@\h:\[\e[32m\]\]\w\[\e[0m\]$(__git_ps1 " (%s)")\n$ '

if type fzf > /dev/null; then
  . /usr/share/fzf/key-bindings.bash
  . /usr/share/fzf/completion.bash
fi
