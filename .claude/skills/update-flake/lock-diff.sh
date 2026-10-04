#!/usr/bin/env bash
# HEAD の flake.lock と作業ツリーの flake.lock を比べ、root input ごとの変更を出力する。
#   changed<TAB><input><TAB><old rev>(<old date>) → <new rev>(<new date>)
#   unchanged<TAB><input>
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

jq -rn \
  --argjson old "$(git show HEAD:flake.lock)" \
  --slurpfile new flake.lock '
  def info($lock; $name):
    $lock.nodes[$lock.nodes.root.inputs[$name]].locked
    | "`\(.rev[0:7])` (\(.lastModified | todate[0:10]))";
  $new[0] as $new
  | $new.nodes.root.inputs | keys[] as $name
  | info($old; $name) as $o
  | info($new; $name) as $n
  | if $o == $n then "unchanged\t\($name)"
    else "changed\t\($name)\t\($o) → \($n)" end
'
