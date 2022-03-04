# Changing Directories
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_MINUS

# History
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt SHARE_HISTORY

# Input/Output
setopt CORRECT
setopt CORRECT_ALL

# homebrew
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

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# rbenv
if type rbenv &>/dev/null; then
  eval "$(rbenv init -)"
fi
