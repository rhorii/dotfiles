{
  pkgs,
  ...
}:
{
  programs.ghostty = {
    enable = true;
    package = pkgs.ghostty-bin;
    enableZshIntegration = true;
    settings = {
      theme = "iTerm2 Solarized Dark";
      font-family = "HackGen Console NF";
      font-size = 16;
      window-padding-x = 8;
      window-padding-y = 8;
      window-padding-balance = true;
    };
  };
}
