{ ... }:
{
  home.file = {
    "bin/create-new-window-firefox.sh" = {
      source = ../../bin/create-new-window-firefox.sh;
      executable = true;
    };
    "bin/create-new-window-safari.sh" = {
      source = ../../bin/create-new-window-safari.sh;
      executable = true;
    };
    "bin/reconnect-airpods.applescript" = {
      source = ../../bin/reconnect-airpods.applescript;
      executable = true;
    };
  };
}
