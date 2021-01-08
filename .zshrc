typeset -gx -U path
path=(
  /opt/homebrew/bin(N-/)
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
