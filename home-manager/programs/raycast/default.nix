{ pkgs, ... }:
{
  home.packages = [ pkgs.raycast ];

  home.file = {
    ".config/raycast/scripts/create-new-window-firefox.sh" = {
      source = ./scripts/create-new-window-firefox.sh;
      executable = true;
    };
    ".config/raycast/scripts/create-new-window-safari.sh" = {
      source = ./scripts/create-new-window-safari.sh;
      executable = true;
    };
    ".config/raycast/scripts/reconnect-airpods.applescript" = {
      source = ./scripts/reconnect-airpods.applescript;
      executable = true;
    };
  };
}
