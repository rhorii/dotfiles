#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/bin:${PATH}"

if type rbenv > /dev/null; then
  eval "$(rbenv init -)"
fi
