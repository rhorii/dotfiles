typeset -gx -U path
path=(
  /opt/homebrew/bin(N-/)
  /opt/homebrew/sbin(N-/)
  $path[@]
)

if type brew &>/dev/null; then
  fpath=(
    $(brew --prefix)/share/zsh/site-functions
    $(brew --prefix)/share/zsh-completions
    $fpath
  )

  autoload -Uz compinit
  compinit
fi

if type rbenv &>/dev/null; then
  eval "$(rbenv init -)"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
