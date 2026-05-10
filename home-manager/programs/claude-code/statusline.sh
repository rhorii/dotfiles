#!/bin/sh
# Claude Code ステータスライン表示スクリプト
#
# 出力例:
#   🤖 Claude Opus 4 | 🧠 [▓▓▓▓▓▓░░░░░░░░░░░░░░] 30% | 💰 5h [▓▓░░░░░░░░░░░░░░░░░░] 10% (🔄 14:30) | 7d [▓░░░░░░░░░░░░░░░░░░░] 5% (🔄 03/25 09:00)
#
# 標準入力: Claude Code が提供するJSON (model, context_window, rate_limits)
# 標準出力: ANSIカラー付きの1行テキスト

# --- 入力のパース ---

input=$(cat)
eval "$(echo "$input" | jq -r '
  @sh "MODEL=\(.model.display_name)",
  @sh "CTX_USED=\(.context_window.used_percentage // empty | tostring | split(".")[0])",
  @sh "FIVE_HOUR=\(.rate_limits.five_hour.used_percentage // empty | tostring | split(".")[0])",
  @sh "FIVE_HOUR_RESET=\(.rate_limits.five_hour.resets_at // empty | tostring)",
  @sh "SEVEN_DAY=\(.rate_limits.seven_day.used_percentage // empty | tostring | split(".")[0])",
  @sh "SEVEN_DAY_RESET=\(.rate_limits.seven_day.resets_at // empty | tostring)"
')"

# --- ANSIカラー定義 ---

BOLD="\033[1m"
GREEN="\033[32m"
RESET="\033[0m"

# --- ユーティリティ関数 ---

# パーセンテージ(0-100)をプログレスバーに変換する
# 例: make_bar 30 → ▓▓▓▓▓▓░░░░░░░░░░░░░░
make_bar() {
  BAR_WIDTH=20
  FILLED=$(($1 * BAR_WIDTH / 100))
  i=0
  while [ "$i" -lt "$FILLED" ]; do
    printf '▓'
    i=$((i + 1))
  done
  i=0
  while [ "$i" -lt "$((BAR_WIDTH - FILLED))" ]; do
    printf '░'
    i=$((i + 1))
  done
}

# Unixエポック秒を指定フォーマットの日時文字列に変換する
# 例: format_time 1711234567 "%H:%M" → 14:30
format_time() {
  epoch="$1" fmt="$2"
  if [ -n "$epoch" ] && [ "$epoch" != "null" ]; then
    date -d "@$epoch" "+$fmt" 2>/dev/null
  fi
}

# レート制限の1項目を組み立てる
# 例: format_rate_part "5h" "10" "14:30" → 5h [▓▓░░░░░░░░░░░░░░░░░░] 10% (🔄 14:30)
format_rate_part() {
  label="$1" pct="$2" reset_fmt="$3"
  PART="${label} $(make_bar "$pct") ${GREEN}${pct}%${RESET}"
  [ -n "$reset_fmt" ] && PART="${PART} (🔄 ${reset_fmt})"
  printf "%s" "$PART"
}

# --- 出力 ---

# モデル | コンテキスト使用率 | レート制限(5h/7d)
printf "🤖 %b%s%b" "${BOLD}${GREEN}" "$MODEL" "$RESET"

if [ -n "$CTX_USED" ]; then
  printf " | 🧠 %s %b%s%b%%" "$(make_bar "$CTX_USED")" "$GREEN" "$CTX_USED" "$RESET"
fi

RATE_LINE=""
if [ -n "$FIVE_HOUR" ]; then
  RATE_LINE=$(format_rate_part "5h" "$FIVE_HOUR" "$(format_time "$FIVE_HOUR_RESET" "%H:%M")")
fi
if [ -n "$SEVEN_DAY" ]; then
  [ -n "$RATE_LINE" ] && RATE_LINE="${RATE_LINE} | "
  RATE_LINE="${RATE_LINE}$(format_rate_part "7d" "$SEVEN_DAY" "$(format_time "$SEVEN_DAY_RESET" "%m/%d %H:%M")")"
fi
[ -n "$RATE_LINE" ] && printf " | 💰 %b" "$RATE_LINE"
