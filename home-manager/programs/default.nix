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
    ./emacs.nix
    ./zed.nix
  ];
}
