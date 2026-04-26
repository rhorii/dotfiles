{
  description = "Home Manager configuration of rhorii";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nix-darwin, home-manager, ... }:
    {
      darwinConfigurations."hank" = nix-darwin.lib.darwinSystem {
        modules = [
          ./nix/darwin
          home-manager.darwinModules.home-manager
        ];
      };
    };
}
