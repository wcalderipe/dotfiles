#!/bin/bash

for lang in ruby node bun go; do
  log_info "installing dev env: $lang"
  omarchy-install-dev-env "$lang"
done
