#!/bin/bash
# Based on https://gist.github.com/Mohamed3on/70780575570a07985916e5f50e290382

export TERM=xterm-256color

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name // empty')
output_style=$(echo "$input" | jq -r '.output_style.name // empty')

# Token usage
input_tokens=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
output_tokens=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
context_limit=$(echo "$input" | jq -r '.context_window.context_window_size // 0')
total_tokens=$((input_tokens + output_tokens))

# Lines changed
lines_added=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
lines_removed=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')

# Colors
RED=$'\033[31m'
GREEN=$'\033[32m'
YELLOW=$'\033[33m'
BLUE=$'\033[34m'
MAGENTA=$'\033[35m'
GRAY=$'\033[90m'
RESET=$'\033[0m'

SEP="${GRAY}|${RESET}"

short_dir=$(basename "$cwd")

# Lines changed
lines_info=""
if [ "$lines_added" -gt 0 ] || [ "$lines_removed" -gt 0 ] 2>/dev/null; then
  net=$((lines_added - lines_removed))
  if [ "$net" -gt 0 ]; then
    net_symbol="${GREEN}▲${RESET}"
  elif [ "$net" -lt 0 ]; then
    net_symbol="${RED}▼${RESET}"
  else
    net_symbol="${GRAY}=${RESET}"
  fi
  lines_info=" ${SEP} ${net_symbol} ${GREEN}+${lines_added}${RESET} ${RED}-${lines_removed}${RESET}"
fi

# Token progress bar
token_info=""
if [ "$total_tokens" -gt 0 ] 2>/dev/null; then
  if [ "$context_limit" -gt 0 ] 2>/dev/null; then
    pct=$((total_tokens * 100 / context_limit))
    if [ "$pct" -ge 75 ]; then
      bar_color="$RED"
    elif [ "$pct" -ge 50 ]; then
      bar_color="$YELLOW"
    else
      bar_color="$GREEN"
    fi
    bar_width=8
    filled=$((pct * bar_width / 100))
    [ "$filled" -gt "$bar_width" ] && filled=$bar_width
    [ "$filled" -lt 1 ] && [ "$pct" -gt 0 ] && filled=1
    empty=$((bar_width - filled))
    bar="${bar_color}"
    for ((i = 0; i < filled; i++)); do bar+="▓"; done
    bar+="${GRAY}"
    for ((i = 0; i < empty; i++)); do bar+="░"; done
    bar+="${RESET}"
    token_info=" ${SEP} ${bar} ${GRAY}${pct}%${RESET}"
  fi
fi

# Model with tier colors
model_info=""
if [ -n "$model" ]; then
  case "$model" in
    *Opus*) model_color="$MAGENTA"; model_symbol="◆" ;;
    *Sonnet*) model_color="$BLUE"; model_symbol="◇" ;;
    *Haiku*) model_color="$GREEN"; model_symbol="○" ;;
    *) model_color="$GRAY"; model_symbol="●" ;;
  esac
  model_info=" ${SEP} ${model_color}${model_symbol} ${model}${RESET}"
fi

# Output style
style_info=""
if [ -n "$output_style" ] && [ "$output_style" != "null" ]; then
  style_info=" ${SEP} ${GRAY}${output_style}${RESET}"
fi

printf "${BLUE}${short_dir}${RESET}%s%s%s%s" "$lines_info" "$token_info" "$model_info" "$style_info"
