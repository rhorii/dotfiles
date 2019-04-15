#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if type rbenv > /dev/null; then
  eval "$(rbenv init -)"
fi
