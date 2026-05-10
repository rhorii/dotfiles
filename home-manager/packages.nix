{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # Git
    gh
    ghq

    # Shell
    bat

    # Finance
    ledger
  ];
}
