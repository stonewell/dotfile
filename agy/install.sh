#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-workspace}"

if [ "$MODE" = "global" ]; then
  DEST_AGENTS="$HOME/.gemini/config/agents"
  DEST_SETTINGS="$HOME/.gemini/antigravity-cli"
  echo "Installing globally to $DEST_AGENTS and $DEST_SETTINGS..."
  mkdir -p "$DEST_AGENTS" "$DEST_SETTINGS"
  cp -r agents/agents/* "$DEST_AGENTS/"
  cp settings.json "$DEST_SETTINGS/settings.json"
  cp AGENTS.md "$HOME/.gemini/GEMINI.md"
  echo "Global configuration installed."
else
  echo "Installing to current workspace..."
  mkdir -p .agents/agents .agents/hooks
  cp -r agents/agents/* .agents/agents/
  cp agents/hooks.json .agents/
  cp agents/hooks/pro_quota_router.py .agents/hooks/
  cp AGENTS.md ./AGENTS.md
  chmod +x .agents/hooks/pro_quota_router.py
  echo "Workspace configuration installed (AGENTS.md + .agents/)."
fi

echo "=== Setup complete! Start Antigravity with simply: agy ==="
