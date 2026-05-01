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

    initContent = ''
      ghq-fzf() {
        local repo
        repo=$(ghq list | fzf \
          --preview "bat --color=always --style=plain \$(ghq root)/{}/README.md 2>/dev/null || ls -la \$(ghq root)/{}" \
          --prompt="repo> " \
          --height 40% \
          --layout=reverse \
          --border)
        if [ -n "$repo" ]; then
          BUFFER="cd -- \$(ghq root)/$repo"
          zle accept-line
        fi
        zle reset-prompt
      }
      zle -N ghq-fzf
      bindkey "^g" ghq-fzf
    '';
  };
}
