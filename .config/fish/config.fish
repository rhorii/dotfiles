# golang
set -x GOPATH $HOME

# rbenv
status --is-interactive; and source (rbenv init -|psub)

# PATH
test -d $HOME/.local/bin; and set -x PATH $HOME/.local/bin $PATH
