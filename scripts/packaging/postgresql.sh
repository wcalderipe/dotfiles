#!/bin/bash

pkg_install postgresql

if [[ -z "$(sudo ls -A /var/lib/postgres/data 2>/dev/null)" ]]; then
  sudo -u postgres initdb --locale=C.UTF-8 --encoding=UTF8 -D /var/lib/postgres/data --data-checksums
fi

sudo systemctl enable --now postgresql

if ! sudo -u postgres psql -tAc "SELECT 1 FROM pg_roles WHERE rolname='$USER'" | grep -q 1; then
  sudo -u postgres createuser --superuser "$USER"
fi

if ! psql -lqt | cut -d'|' -f1 | grep -qw "$USER"; then
  createdb "$USER"
fi
