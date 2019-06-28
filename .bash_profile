#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if type rbenv > /dev/null; then
  eval "$(rbenv init -)"
fi

export PATH="${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/bin:${PATH}"
