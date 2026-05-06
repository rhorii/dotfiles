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

    # Raycast
    ./raycast

    # Terminal
    ./ghostty.nix
  ];
}
