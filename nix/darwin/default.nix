{
  ...
}:
{
  nixpkgs.hostPlatform = "aarch64-darwin";
  system.stateVersion = 6;
  nix.enable = false;

  users.users.rhorii = {
    name = "rhorii";
    home = "/Users/rhorii";
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.rhorii = import ../home;
  };
}
