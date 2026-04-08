#!/bin/bash

if command -v claude &>/dev/null; then
  log_info "already installed: claude-code"
else
  log_info "installing: claude-code"
  curl -fsSL https://claude.ai/install.sh | bash
fi
