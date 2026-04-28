{
  username,
  ...
}:
{
  system.stateVersion = 6;
  nixpkgs.hostPlatform = "aarch64-darwin";
  nix.enable = false;

  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };
}
