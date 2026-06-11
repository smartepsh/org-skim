# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository scope

Single-file Emacs Lisp package (`org-skim.el`) that drives the Skim PDF reader from Emacs via AppleScript. There is no build system, no test suite, and no `Cask`/`Eldev` config — everything lives in one file plus `README.org`.

## Sanity-check commands

```sh
emacs -Q --batch -f batch-byte-compile org-skim.el   # catches syntax + docstring lint errors; produces org-skim.elc (delete it after)
emacs -Q --batch -L . -l org-skim.el                 # load-time smoke test
```

There are no automated tests. Manual verification requires macOS with Skim and a PDF open.

## Architecture

The package layers three concerns top-to-bottom in `org-skim.el`:

1. **`defcustom` block** — user-facing knobs: TOC formatting (`org-skim-toc-header-char`, `org-skim-toc-header-name`) and AppleScript backend selection (`org-skim-applescript-backend`: `auto` / `do-applescript` / `osascript`).

2. **AppleScript dispatch layer** — `org-skim--run-applescript SCRIPT &rest ARGV` is the single entry point for executing any AppleScript. It resolves the backend via `org-skim--resolved-backend` and:
   - For `do-applescript`: wraps SCRIPT with a synthesized `run {...}` call (`org-skim--applescript-with-argv`) so scripts that use `on run argv` work identically through both backends.
   - For `osascript`: shells out via `call-process-region`, feeding the script on stdin (`-`) and passing ARGV as positional args.

   All AppleScript-driven functions must go through this helper. Do not call `do-applescript` or `osascript` directly anywhere else — that would bypass the backend abstraction.

3. **Feature functions** — TOC extraction (`org-skim--toc-string` → `org-skim-insert-toc` / `org-skim-yank-toc`) and page navigation (`org-skim-current-page`, `org-skim-next-page`, `org-skim-previous-page`). Each is a thin Elisp wrapper around an AppleScript string passed to `org-skim--run-applescript`.

The TOC AppleScript (`org-skim--toc-applescript`) is the only large embedded script and uses `on run argv` to receive the header character and base level; the navigation scripts are small enough to inline directly.

## Conventions specific to this package

- Public functions are `org-skim-*`; private helpers and the embedded AppleScript constant are `org-skim--*`. Maintain this split.
- Interactive commands carry `;;;###autoload`. `org-skim-current-page` is intentionally non-interactive (returns a value).
- AppleScript strings begin every Skim interaction with `if (count of documents) is 0 then error "No document open in Skim."` so the Elisp side surfaces a useful error rather than an opaque AppleScript failure.
- Page navigation is bounded inside AppleScript (`if n < (count of pages)` / `if n > 1`) — do not move the bounds check to Elisp, since it would require an extra round trip.
- When extending: prefer reusing `org-skim--run-applescript` over adding a new dispatch path. If a new script needs arguments, use `on run argv` so it works through both backends.
