{ ... }:
{
  programs.zsh = {
    enable = true;
  };

  programs.starship = {
    enable = true;
  };

  programs.git = {
    enable = true;

    settings = {
      status.showUntrackedFiles = "all";
      ghq.root = "~/src";
    };
  };
}
