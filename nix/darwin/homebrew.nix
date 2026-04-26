{ ... }:
{
  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = false;
      cleanup = "zap";
      upgrade = true;
    };

    taps = [ ];

    brews = [
      "asdf"
      "fzf"
    ];

    casks = [ ];

    masApps = { };
  };
}
