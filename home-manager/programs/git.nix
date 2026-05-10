{ ... }:
{
  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Ryota Horii";
        email = "rhorii@gmail.com";
      };
      init.defaultBranch = "main";
      status.showUntrackedFiles = "all";
      pull.rebase = true;
      push.autoSetupRemote = true;
      ghq.root = "~/src";
    };

    ignores = [
      ".direnv/"
      ".envrc.local"
    ];
  };
}
