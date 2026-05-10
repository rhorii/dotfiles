{ ... }:
{
  programs.git = {
    enable = true;

    userName = "Ryota Horii";
    userEmail = "rhorii@gmail.com";

    settings = {
      status.showUntrackedFiles = "all";
      ghq.root = "~/src";
      init.defaultBranch = "main";
      pull.rebase = true;
      push.autoSetupRemote = true;
    };

    ignores = [
      ".direnv/"
      ".envrc.local"
    ];
  };
}
