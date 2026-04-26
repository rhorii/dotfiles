{
  ...
}:
{
  imports = [ ../../modules/darwin/common.nix ];

  # Must match the darwinConfigurations attr name in flake.nix.
  networking.hostName = "hank";
  system.stateVersion = 6;

  users.users.rhorii = {
    name = "rhorii";
    home = "/Users/rhorii";
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.rhorii = import ../../users/rhorii;
  };
}
