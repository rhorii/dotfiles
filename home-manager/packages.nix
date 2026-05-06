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
    bat

    # Terminal
    ghostty-bin

    # Finance
    ledger
  ];
}
