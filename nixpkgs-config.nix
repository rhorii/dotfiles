{
  allowUnfreePredicate =
    pkg:
    builtins.elem (pkg.pname or pkg.name) [
      "claude-code"
      "raycast"
    ];
}
