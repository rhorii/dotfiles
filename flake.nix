{
  description = "rhorii's nix-darwin + home-manager configuration";

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
    { nixpkgs, nix-darwin, home-manager, ... }:
    let
      system = "aarch64-darwin";
      hostname = "hank";
      username = "rhorii";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      darwinConfigurations.${hostname} = nix-darwin.lib.darwinSystem {
        specialArgs = { inherit username; };
        modules = [
          ./nix/darwin
        ];
      };

      homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgs;
        extraSpecialArgs = { inherit username; };
        modules = [ ./nix/home ];
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          nixd
          nixfmt-rfc-style
        ];
      };
    };
}
