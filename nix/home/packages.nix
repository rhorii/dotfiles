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
  ];
}
