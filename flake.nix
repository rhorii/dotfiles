{
  description = "rhorii's nix-darwin + home-manager configurations";

  inputs = {
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
    let
      mkDarwin =
        {
          hostname,
          system ? "aarch64-darwin",
        }:
        nix-darwin.lib.darwinSystem {
          modules = [
            { nixpkgs.hostPlatform = system; }
            ./nix/hosts/${hostname}
            home-manager.darwinModules.home-manager
          ];
        };
    in
    {
      darwinConfigurations.hank = mkDarwin { hostname = "hank"; };
    };
}
