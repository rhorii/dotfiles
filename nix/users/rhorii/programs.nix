{
  config,
  ...
}:
{
  programs.git = {
    enable = true;

    settings = {
      status.showUntrackedFiles = "all";
      ghq.root = "~/src";
    };
  };
}
