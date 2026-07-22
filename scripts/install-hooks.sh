#!/usr/bin/env bash
#
# Install the git hooks. Run once per clone:
#
#   scripts/install-hooks.sh
#
# Hooks live in .git/hooks, which git does not track, so they cannot ship with
# the repo -- hence an installer rather than a committed hook. Re-running is
# safe; an existing hook this script did not write is left alone rather than
# overwritten, because a hook you did not expect to lose is worse than a hook
# that failed to install.
set -euo pipefail
cd "$(dirname "$0")/.."

hooks_dir="$(git rev-parse --git-path hooks)"
mkdir -p "$hooks_dir"
target="$hooks_dir/pre-commit"
marker="# installed by scripts/install-hooks.sh"

if [ -e "$target" ] && ! grep -qF "$marker" "$target"; then
  echo "A pre-commit hook already exists and was not installed by this script:"
  echo "  $target"
  echo "Leaving it alone. Merge it by hand if you want both."
  exit 1
fi

cat > "$target" <<'HOOK'
#!/usr/bin/env bash
# installed by scripts/install-hooks.sh
#
# The FAST half of scripts/precheck.sh: the README version check and lintr,
# which together take a few seconds. The suite, the smoke test and the render
# are deliberately not here -- a pre-commit hook that takes minutes is a hook
# people learn to pass --no-verify to, and a bypassed hook checks nothing.
#
# Run the rest before pushing:  scripts/precheck.sh
# Skip this once:               git commit --no-verify
exec scripts/precheck.sh readme lint
HOOK
chmod +x "$target"

echo "Installed $target"
echo "  runs: scripts/precheck.sh readme lint   (a few seconds)"
echo "  before pushing, run the full set: scripts/precheck.sh"
