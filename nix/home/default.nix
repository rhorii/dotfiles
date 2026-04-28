{
  username,
  ...
}:
{
  imports = [
    ./packages.nix
    ./programs
    ./scripts.nix
  ];

  home.username = username;
  home.homeDirectory = "/Users/${username}";

  # Do not change without reading the Home Manager release notes first.
  home.stateVersion = "25.11";

  programs.home-manager.enable = true;
}
