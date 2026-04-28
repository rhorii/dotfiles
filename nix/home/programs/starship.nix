{ ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      command_timeout = 1000;
      battery.disabled = true;
    };
  };
}
