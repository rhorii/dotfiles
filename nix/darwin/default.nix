{
  username,
  ...
}:
{
  imports = [
    ./homebrew.nix
  ];

  system = {
    stateVersion = 6;
    primaryUser = username;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
  nix.enable = false;

  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };
}
