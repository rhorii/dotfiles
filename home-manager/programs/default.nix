{ ... }:
{
  imports = [
    # Shell
    ./zsh.nix
    ./fzf.nix
    ./starship.nix
    ./direnv.nix

    # VCS
    ./git.nix

    # Editors
    ./emacs
    ./zed.nix

    # Terminal
    ./ghostty.nix

    # Claude Code
    ./claude-code.nix

    # Raycast
    ./raycast

    # Input
    ./karabiner
  ];
}
