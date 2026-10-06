#!/bin/bash
cd -- "$(dirname -- "$0")" || exit 1
rscript="$(command -v Rscript 2>/dev/null)"
if [ -z "$rscript" ]; then
  for candidate in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript /Library/Frameworks/R.framework/Resources/bin/Rscript; do
    if [ -x "$candidate" ]; then rscript="$candidate"; break; fi
  done
fi
if [ -z "$rscript" ]; then
  printf 'R was not found. Install R from https://cran.r-project.org/bin/macosx/ then retry.\n'
  read -r -p 'Press Enter to close.'
  exit 1
fi
"$rscript" --vanilla scripts/start_core.R "$@"
result=$?
if [ "$result" -ne 0 ]; then read -r -p 'Startup stopped. Press Enter to close.'; fi
exit "$result"
