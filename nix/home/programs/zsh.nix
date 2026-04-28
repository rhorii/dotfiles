{ config, ... }:
{
  programs.zsh = {
    enable = true;

    autocd = true;
    dotDir = "${config.xdg.configHome}/zsh";

    shellAliases = {
      la = "ls -alh";
      ll = "ls -lh";
      ls = "ls -G";
    };

    history = {
      append = true;
      size = 1000000;
      save = 1000000;
      ignoreDups = true;
      ignoreAllDups = true;
      saveNoDups = true;
      ignoreSpace = true;
      expireDuplicatesFirst = true;
      extended = true;
      share = true;
    };
  };
}
