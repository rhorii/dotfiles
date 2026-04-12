{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    git
    ghq
  ];
}
