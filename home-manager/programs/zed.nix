{ ... }:
{
  programs.zed-editor = {
    enable = true;
    package = null;
    mutableUserSettings = true;
    userSettings = {
      base_keymap = "Emacs";
      ui_font_size = 12;
      buffer_font_size = 12;
      terminal = {
        font_family = "HackGen Console NF";
        font_size = 12.0;
      };
      theme = {
        mode = "system";
        light = "One Light";
        dark = "One Dark";
      };
      project_panel.dock = "right";
      outline_panel.dock = "right";
      collaboration_panel.dock = "right";
      git_panel = {
        dock = "right";
        tree_view = true;
      };
      agent = {
        dock = "left";
        favorite_models = [ ];
        model_parameters = [ ];
      };
    };
  };
}
