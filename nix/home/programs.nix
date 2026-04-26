{ ... }:
{
  programs.zsh = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
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
