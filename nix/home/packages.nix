{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    # Git
    git
    ghq
  ];
}
