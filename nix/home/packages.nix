{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # Git
    git
    gh
    ghq

    # Shell
    zsh
    fzf
    starship

    # Finance
    ledger

    # Nix
    nixd
  ];
}
