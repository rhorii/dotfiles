{ ... }:
{
  programs.claude-code = {
    enable = true;
    settings = {
      autoUpdaterStatus = "disabled";
      language = "japanese";
      skipAutoPermissionPrompt = true;
      statusLine = {
        type = "command";
        command = "~/.claude/statusline.sh";
      };
    };
  };

  home.file.".claude/statusline.sh" = {
    source = ./statusline.sh;
    executable = true;
  };
}
