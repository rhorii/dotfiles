# golang
set -x GOPATH $HOME

# rbenv
if status --is-interactive; and type --quiet rbenv
    source (rbenv init -|psub)
end

# key bindings
fzf_key_bindings
