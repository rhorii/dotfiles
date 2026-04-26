{
  description = "rhorii's nix-darwin + home-manager configuration";

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
    { nixpkgs, nix-darwin, home-manager, ... }:
    let
      hostname = "hank";
      username = "rhorii";
      system = "aarch64-darwin";
    in
    {
      darwinConfigurations.${hostname} = nix-darwin.lib.darwinSystem {
        specialArgs = { inherit username hostname; };

        modules = [
          ./nix/darwin
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup";
              extraSpecialArgs = { inherit username; };
              users.${username} = import ./nix/home;
            };
          }
        ];
      };

      # homeConfigurations."rhorii" = home-manager.lib.homeManagerConfiguration {
      #   inherit pkgs;

      #   # Specify your home configuration modules here, for example,
      #   # the path to your home.nix.
      # modules = [ ./nix/home ];

      #   # Optionally use extraSpecialArgs
      #   # to pass through arguments to home.nix
      # };
    };
}
