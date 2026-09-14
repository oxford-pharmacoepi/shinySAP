#!/usr/bin/env bash
#
# Every gate CI applies, as steps that can be run one at a time.
#
# THIS SCRIPT IS THE SINGLE SOURCE. The workflows call it rather than restating
# the commands, so a check cannot pass locally and fail on GitHub because the two
# drifted. The workflow still owns the ENVIRONMENTS -- which R and which library
# -- because that is what a CI runner is for; this owns what "passing" means.
#
#   scripts/precheck.sh                  every step
#   scripts/precheck.sh readme lint      only those steps
#   scripts/precheck.sh --fix            fix what is mechanically fixable first
#
# The repo is ONE package. The Shiny app lives in inst/app as a plain Shiny app
# directory, and the package's testthat suite covers both (test-sap-*.R for the
# package, test-app-*.R for the app), so `tests` checks everything at once.
#
# --fix only touches things with exactly one right answer -- today the README's
# schema version, derived from data-raw/. Lints and failing tests are never
# auto-fixed: a linter that rewrites your code is one you stop reading.
set -uo pipefail
cd "$(dirname "$0")/.."

ALL_STEPS=(readme lint parse install tests smoke render)
FIX=0
STEPS=()
for arg in "$@"; do
  case "$arg" in
    --fix) FIX=1 ;;
    --help|-h) sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) STEPS+=("$arg") ;;
  esac
done
[ ${#STEPS[@]} -eq 0 ] && STEPS=("${ALL_STEPS[@]}")

# An unknown step must be an ERROR, never a no-op: a typo in a workflow would
# otherwise report success while checking nothing.
for requested in "${STEPS[@]}"; do
  if [[ " ${ALL_STEPS[*]} " != *" $requested "* ]]; then
    printf '\033[31munknown step: %s\033[0m\n' "$requested" >&2
    printf 'known steps: %s\n' "${ALL_STEPS[*]}" >&2
    exit 2
  fi
done

# rmarkdown needs a pandoc; a CI runner ships one, a Mac usually only has
# RStudio's. Left alone if the environment already names one.
if [ -z "${RSTUDIO_PANDOC:-}" ]; then
  for candidate in \
    /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64 \
    /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64 \
    /usr/lib/rstudio/bin/quarto/bin/tools; do
    [ -d "$candidate" ] && export RSTUDIO_PANDOC="$candidate" && break
  done
fi

FAILED=0
step() { printf '\n\033[1m== %s\033[0m\n' "$1"; }
ok()   { printf '\033[32mok\033[0m %s\n' "$1"; }
bad()  { printf '\033[31mFAIL\033[0m %s\n' "$1"; FAILED=1; }

# The current schema version is the highest data-raw/v<version> directory; the
# README's example JSON must carry it.
schema_version() {
  ls data-raw | sed -n 's/^v//p' | sort -V | tail -1
}

# One scratch library shared by install, tests, smoke and render, so the app
# under test is the INSTALLED copy -- system.file("app") must find it.
LIB=""
ensure_lib() {
  [ -n "$LIB" ] && return 0
  LIB=$(mktemp -d)
  if R CMD INSTALL --no-docs --no-multiarch --library="$LIB" . > "$LIB/install.log" 2>&1; then
    ok "installed into $LIB"
  else
    tail -30 "$LIB/install.log"; bad "R CMD INSTALL"
    return 1
  fi
}

for s in "${STEPS[@]}"; do
  case "$s" in

    readme)
      step "readme: README carries the current schema version"
      version=$(schema_version)
      if [ -z "$version" ]; then bad "no data-raw/v<version> directory"; continue; fi
      if grep -q "\"sap_schema_version\": \"$version\"" README.Rmd; then
        ok "README.Rmd example is schema $version"
      elif [ "$FIX" -eq 1 ]; then
        perl -0pi -e "s/\"sap_schema_version\": \"[0-9.]+\"/\"sap_schema_version\": \"$version\"/g" README.Rmd
        ok "README.Rmd example rewritten to schema $version (re-knit README.md)"
      else
        bad "README.Rmd example JSON does not say \"sap_schema_version\": \"$version\" (--fix rewrites it)"
      fi
      ;;

    lint)
      step "lint: lintr over the package, the app and the tests"
      out=$(R_PROFILE_USER=/dev/null Rscript -e '
        suppressMessages(library(lintr))
        cat("lintr", as.character(packageVersion("lintr")), "\n")
        l <- lint_dir(".")
        print(l)
        quit(status = as.integer(length(l) > 0))' 2>&1)
      if [ $? -eq 0 ]; then ok "no lints"; else echo "$out"; bad "lint"; fi
      ;;

    parse)
      step "parse: every R file parses, and the app never calls the package unqualified"
      out=$(Rscript -e 'files <- c(list.files("R", pattern = "[.]R$", full.names = TRUE),
                                   list.files("inst/app/R", pattern = "[.]R$", full.names = TRUE),
                                   "inst/app/app.R")
                        for (f in files) parse(f)
                        cat(length(files), "files parse\n")' 2>&1)
      if [ $? -eq 0 ]; then ok "$out"; else echo "$out"; bad "parse"; fi
      # Under R CMD check the app's tests can reach package internals by
      # accident; under runApp() they cannot. Every call must be shinySAP::.
      exported=$(sed -n 's/^export(\(.*\))$/\1/p' NAMESPACE | paste -sd'|' -)
      bare=$(grep -nE "(^|[^:A-Za-z0-9_.])($exported)\(" inst/app/R/*.R inst/app/app.R | grep -vE "^\S+:\s*#" || true)
      if [ -z "$bare" ]; then ok "app calls the package as shinySAP::"; else echo "$bare"; bad "unqualified package call in the app"; fi
      ;;

    install)
      step "install: R CMD INSTALL into a scratch library"
      ensure_lib || true
      ;;

    tests)
      step "tests: the package suite (package and app)"
      ensure_lib || continue
      out=$(cd tests && R_LIBS="$LIB:${R_LIBS:-}" Rscript testthat.R 2>&1)
      if [ $? -eq 0 ]; then echo "$out" | tail -3; ok "tests"; else echo "$out" | tail -60; bad "tests"; fi
      ;;

    smoke)
      step "smoke: shinySap() serves a page from the installed package"
      ensure_lib || continue
      log=$(mktemp)
      R_LIBS="$LIB:${R_LIBS:-}" Rscript -e 'shinySAP::shinySap(port = 8123, launch.browser = FALSE, outputDir = tempdir())' > "$log" 2>&1 &
      pid=$!
      up=0
      for _ in $(seq 1 30); do
        if curl -sf -o /dev/null http://127.0.0.1:8123; then up=1; break; fi
        sleep 1
      done
      kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
      if [ "$up" -eq 1 ]; then ok "app answered on :8123"; else cat "$log"; bad "app did not answer within 30s"; fi
      ;;

    render)
      step "render: the preview renders against the fixture (needs pandoc)"
      ensure_lib || continue
      out=$(R_LIBS="$LIB:${R_LIBS:-}" Rscript -e '
        library(shinySAP)
        sap <- readSap("tests/testthat/fixtures/sap-c1-001-v1.0.0.json")
        rmd <- file.path(system.file("app", package = "shinySAP"), "sap_preview.Rmd")
        out <- rmarkdown::render(rmd, output_format = "html_document", output_file = tempfile(fileext = ".html"),
                                 params = list(sap = sap), envir = new.env(), quiet = TRUE)
        cat("rendered", out, "\n")' 2>&1)
      if [ $? -eq 0 ]; then ok "$out"; else echo "$out" | tail -30; bad "render"; fi
      ;;
  esac
done

exit $FAILED
