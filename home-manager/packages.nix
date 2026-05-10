{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # Git
    ghq

    # Shell
    bat

    # Finance
    ledger
  ];
}
