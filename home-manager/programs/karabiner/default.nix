{ pkgs, ... }:
{
  home.packages = [ pkgs.karabiner-elements ];

  home.file.".config/karabiner/karabiner.json" = {
    source = ./karabiner.json;
  };
}
