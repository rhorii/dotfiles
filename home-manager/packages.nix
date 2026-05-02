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

    # Finance
    ledger

  ];
}
