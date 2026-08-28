#!/usr/bin/env bash
#
# Every gate CI applies, as steps that can be run one at a time.
#
# THIS SCRIPT IS THE SINGLE SOURCE. .github/workflows/ci.yaml calls it rather
# than restating the commands, so a check cannot pass locally and fail on
# GitHub because the two drifted. The workflow still owns the ENVIRONMENTS --
# which R and which library -- because that is what a CI runner is for; this
# owns what "passing" means.
#
#   scripts/precheck.sh                  every step
#   scripts/precheck.sh readme lint      only those steps
#   scripts/precheck.sh --fix            fix what is mechanically fixable first
#
# The two CI jobs run different subsets because they have different libraries:
#
#   test job  project dependencies   ->  parse tests apptests smoke
#   lint job  stock library + lintr  ->  readme lint
#
# The repo holds TWO things: the package at the root (R/, tests/) and the frozen
# Shiny app in app/, which is a shiny app DIRECTORY, not a package. They are
# checked separately -- `tests` is the package, `apptests` is the app.
#
# --fix only touches things with exactly one right answer -- today the README's
# schema version, mechanically derived from R/app.R. Lints and failing tests are
# never auto-fixed: a linter that rewrites your code is one you stop reading.
set -uo pipefail
cd "$(dirname "$0")/.."

ALL_STEPS=(readme lint parse tests apptests smoke render)
FIX=0
STEPS=()
for arg in "$@"; do
  case "$arg" in
    --fix) FIX=1 ;;
    --help|-h) sed -n '2,25p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) STEPS+=("$arg") ;;
  esac
done
[ ${#STEPS[@]} -eq 0 ] && STEPS=("${ALL_STEPS[@]}")

# An unknown step must be an ERROR, never a no-op. Skipping it silently would
# make a typo in ci.yaml -- `scripts/precheck.sh redme lint` -- report success
# while checking nothing, which is the one way a green CI can lie.
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

failed=()
step() { printf '\n\033[1m== %s\033[0m\n' "$1"; }
ok()   { printf '   \033[32mOK\033[0m   %s\n' "$1"; }
bad()  { printf '   \033[31mFAIL\033[0m %s\n' "$1"; failed+=("$1"); }
wants() { [[ " ${STEPS[*]} " == *" $1 "* ]]; }

# The README's example JSON must carry the schema version app/app.R defines.
#
# A step of the CI "lint" job but NOT a lint, which is worth knowing: it is the
# one that fails whenever a schema bump lands without the README following it,
# and a red X on a job called "lint" reads as a style problem when it is not.
if wants readme; then
  step "README documents the current schema version"
  version=$(sed -nE 's/^SAP_SCHEMA_VERSION <- "([0-9.]+)"/\1/p' app/app.R)
  if [ -z "$version" ]; then
    bad "SAP_SCHEMA_VERSION not found in app/app.R"
  elif grep -qF "\"sap_schema_version\": \"$version\"" README.Rmd; then
    ok "README carries $version"
  elif [ "$FIX" = "1" ]; then
    perl -0pi -e "s/\"sap_schema_version\": \"[0-9.]+\"/\"sap_schema_version\": \"$version\"/" README.Rmd
    if grep -qF "\"sap_schema_version\": \"$version\"" README.Rmd; then
      ok "README updated to $version"
    else
      bad "could not update README to $version"
    fi
  else
    bad "README does not carry $version (run with --fix)"
  fi
fi

# lintr, against a STOCK library: lintr is not a runtime dependency, so the
# project startup files must not interfere with the stock lint library.
if wants lint; then
  step "Lint (config in .lintr)"
  # The version is reported because CI installs `any::lintr` and a laptop
  # installs whatever it installed once. A default that changed between the two
  # is exactly how a check passes here and fails on GitHub -- .lintr now states
  # the rules that bit us, but printing the version makes the next such skew
  # something you can see rather than deduce.
  out=$(RENV_CONFIG_AUTOLOADER_ENABLED=false R_PROFILE_USER=/dev/null \
        Rscript -e 'cat("lintr", as.character(packageVersion("lintr")), "\n")
                    l <- lintr::lint_dir("."); print(l)
                    quit(status = if (length(l)) 1 else 0)' 2>&1)
  code=$?
  version_line=$(printf '%s' "$out" | head -1)
  if [ $code -eq 0 ]; then ok "no lints ($version_line)"; else printf '%s\n' "$out"; bad "lintr reported problems"; fi
fi

# The suite only sources part of R/, so this is what catches a syntax error in a
# module the tests never load, or in R/app.R itself.
if wants parse; then
  step "Parse all R sources (package and app)"
  out=$(Rscript -e 'files <- c(list.files("R", pattern = "[.]R$", full.names = TRUE),
                               list.files("app/R", pattern = "[.]R$", full.names = TRUE),
                               "app/app.R")
                    invisible(lapply(files, parse))' 2>&1)
  if [ $? -eq 0 ]; then ok "R/ parses"; else printf '%s\n' "$out" | tail -10; bad "a source file does not parse"; fi
fi

# test_check() loads the INSTALLED package, so the suite needs one. CI gets that
# from check-r-package; locally we install to a throwaway library rather than
# ask the developer to remember -- otherwise this step only ever runs on GitHub,
# which is the exact skew this script exists to prevent.
if wants tests; then
  step "Tests — the package"
  lib=$(mktemp -d)
  if ! inst=$(R CMD INSTALL --no-multiarch --no-docs -l "$lib" . 2>&1); then
    printf '%s\n' "$inst" | tail -15; bad "package would not install"
  else
    # test_check() resolves "testthat/" relative to the working directory, so
    # this runs from tests/ -- which is where R CMD check runs it from too.
    out=$(cd tests && R_LIBS="$lib:${R_LIBS:-}" Rscript testthat.R 2>&1)
    code=$?
    summary=$(printf '%s' "$out" | grep -E "^\[ (FAIL|OK)" | tail -1)
    if [ $code -eq 0 ]; then ok "${summary:-suite passed}"; else printf '%s\n' "$out" | tail -30; bad "tests failed"; fi
  fi
  rm -rf "$lib"
fi

# The app's own suite. It cannot ride on test_check(): app/ has no namespace, so
# the runner sources app/R the way loadSupport() does.
if wants apptests; then
  step "Tests — the app"
  out=$(Rscript app/tests/run.R 2>&1)
  code=$?
  summary=$(printf '%s' "$out" | grep -E "^\[ (FAIL|OK)" | tail -1)
  if [ $code -eq 0 ]; then ok "${summary:-app suite passed}"; else printf '%s\n' "$out" | tail -30; bad "app tests failed"; fi
fi

# The app has to actually start. A module that errors at UI-build time passes
# every check above, because nothing else ever builds the UI.
if wants smoke; then
  step "Smoke test — the app starts and serves HTTP 200"
  log=$(mktemp)
  Rscript -e 'shiny::runApp("app", port = 8123)' > "$log" 2>&1 &
  app_pid=$!
  served=1
  for _ in $(seq 1 30); do
    if curl -sf -o /dev/null http://127.0.0.1:8123; then served=0; break; fi
    kill -0 "$app_pid" 2>/dev/null || break
    sleep 1
  done
  kill "$app_pid" 2>/dev/null
  wait "$app_pid" 2>/dev/null
  if [ $served -eq 0 ]; then ok "app served HTTP 200"; else tail -20 "$log"; bad "app did not come up"; fi
  rm -f "$log"
fi

# Not a CI job: the preview is the one thing a user actually looks at, and a
# broken template passes lint, parse and the suite without complaint.
if wants render; then
  step "Preview renders (HTML)"
  out=$(Rscript -e '
    setwd("app")
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
    for (f in c("utils.R", "cohort_kinds.R", "analysis_registry.R", "sap_code.R",
                "cohort_operations.R")) source(file.path("R", f))
    saps <- list.files("tests/fixtures", pattern = "[.]json$", full.names = TRUE)
    if (!length(saps)) { cat("no SAP fixture to render\n"); quit(status = 0) }
    sap <- read_sap(saps[[1]])
    rmarkdown::render("inst/sap_preview.Rmd",
      output_format = rmarkdown::html_document(self_contained = TRUE),
      output_file = tempfile(fileext = ".html"),
      params = list(sap = sap), envir = new.env(parent = globalenv()), quiet = TRUE)
    cat("rendered", basename(saps[[1]]), "\n")' 2>&1)
  if [ $? -eq 0 ]; then ok "$(printf '%s' "$out" | tail -1)"; else printf '%s\n' "$out" | tail -20; bad "preview render failed"; fi
fi

printf '\n'
if [ ${#failed[@]} -eq 0 ]; then
  printf '\033[32mAll checks passed.\033[0m (%s)\n' "${STEPS[*]}"
  exit 0
fi
printf '\033[31m%d check(s) failed:\033[0m\n' "${#failed[@]}"
printf '  - %s\n' "${failed[@]}"
exit 1
