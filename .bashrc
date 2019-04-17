#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export HISTCONTROL=erasedups

if type fzf > /dev/null; then
  . /usr/share/fzf/key-bindings.bash
  . /usr/share/fzf/completion.bash
fi
