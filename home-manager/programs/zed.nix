{ ... }:
{
  programs.zed-editor = {
    enable = true;
    package = null;
    mutableUserSettings = true;
    userSettings = {
      # Appearance
      theme = {
        mode = "system";
        light = "One Light";
        dark = "One Dark";
      };

      buffer_font_family = "HackGen Console NF";
      buffer_font_size = 12;

      # Keymap
      base_keymap = "Emacs";

      # Languages & Tools
      languages = {
        Nix = {
          language_servers = [
            "nixd"
            "!nil"
          ];
        };
      };

      # Panels
      project_panel.dock = "right";
      outline_panel.dock = "right";
      git_panel = {
        dock = "right";
        tree_view = true;
      };
      collaboration_panel.dock = "right";
    };
  };
}
