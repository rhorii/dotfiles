{ pkgs, username, ... }:

let
  email = "change-me@example.com";
  fullName = "Ryota Horii";
in
{
  home.username = username;
  home.homeDirectory = "/Users/${username}";
  home.stateVersion = "24.11";

  home.sessionVariables = {
    EDITOR = "emacs";
    GOPATH = "$HOME";
  };

  home.sessionPath = [
    "$HOME/bin"
    "$HOME/.local/bin"
  ];

  home.packages = with pkgs; [
    fzf
    ghq
    ripgrep
  ];

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = fullName;
    userEmail = email;
    extraConfig = {
      status.showUntrackedFiles = "all";
      ghq.root = "~/src";
    };
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    settings = {
      command_timeout = 1000;
      battery.disabled = true;
    };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    history = {
      size = 1000000;
      save = 1000000;
      extended = true;
      ignoreAllDups = true;
      ignoreSpace = true;
      share = true;
    };
    shellAliases = {
      la = "ls -alh";
      ll = "ls -lh";
      ls = "ls -G";
    };
    initContent = ''
      setopt AUTO_PUSHD
      setopt PUSHD_IGNORE_DUPS
      setopt PUSHD_MINUS
      setopt HIST_REDUCE_BLANKS
      setopt HIST_SAVE_NO_DUPS
      setopt HIST_VERIFY
      setopt CORRECT
      setopt CORRECT_ALL

      typeset -gx -U path
      path=(
        /opt/homebrew/bin(N-/)
        /opt/homebrew/sbin(N-/)
        $path[@]
      )

      if type asdf &>/dev/null; then
        export PATH="''${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
      fi

      if [[ "$INSIDE_EMACS" = 'vterm' ]] \
           && [[ -n ''${EMACS_VTERM_PATH} ]] \
           && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
        source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
        print -Pn "\e]2;%m:%2~\a"
      fi
    '';
  };

  programs.bash = {
    enable = true;
    historySize = 32768;
    historyFileSize = 32768;
    historyControl = [ "ignoreboth" ];
    shellAliases = {
      la = "ls -alh";
      ll = "ls -lh";
      ls = "ls -G";
    };
  };
}
