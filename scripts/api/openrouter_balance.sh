#!/bin/bash

API_KEY="${1:-$OPENROUTER_API_KEY}"

[[ -z "$API_KEY" ]] && echo "Usage: $0 [API_KEY]" && exit 1

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/openrouter"
CACHE_FILE="$CACHE_DIR/balance.cache"
CACHE_TTL=60

mkdir -p "$CACHE_DIR"

if [[ -f "$CACHE_FILE" ]]; then
    cache_age=$(($(date +%s) - $(stat -f %m "$CACHE_FILE" 2>/dev/null || stat -c %Y "$CACHE_FILE" 2>/dev/null)))
    if [[ $cache_age -lt $CACHE_TTL ]]; then
        cat "$CACHE_FILE"
        exit 0
    fi
fi

result=$(curl -s -H "Authorization: Bearer $API_KEY" "https://openrouter.ai/api/v1/credits" | \
  jq '(.data.total_credits - .data.total_usage) * 100 | round / 100')

echo "$result" > "$CACHE_FILE"
echo "$result"
