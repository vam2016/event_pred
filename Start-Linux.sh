#!/bin/sh
cd -- "$(dirname -- "$0")" || exit 1
if ! command -v Rscript >/dev/null 2>&1; then
  printf 'Rscript was not found. Install R first.\n'
  exit 1
fi
exec Rscript --vanilla scripts/start_core.R "$@"
