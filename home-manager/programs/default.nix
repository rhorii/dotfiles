{ ... }:
{
  imports = [
    # Shell
    ./zsh.nix
    ./fzf.nix
    ./starship.nix
    ./direnv.nix
    ./mise.nix

    # VCS
    ./git.nix
    ./gh.nix

    # Editors
    ./emacs
    ./zed.nix

    # Terminal
    ./ghostty.nix

    # Claude Code
    ./claude-code

    # Raycast
    ./raycast

    # Input
    ./karabiner
  ];
}
