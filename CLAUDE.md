# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository scope

Small Emacs Lisp package that drives the Skim PDF reader from Emacs via AppleScript. There is no build system, no test suite, and no `Cask`/`Eldev` config. The code is split across three files plus `README.org`:

- `org-skim-helpers.el` — shared infrastructure (customization group, AppleScript dispatch, string/template utilities). No `org-skim-*` deps.
- `org-skim.el` — TOC, page navigation, page links, reading bar. Loads `org-skim-helpers` and pulls in `org-skim-bookmarks` so a single `(require 'org-skim)` gives users every command.
- `org-skim-bookmarks.el` — bookmark commands (`org-skim-bookmark-add` / `-remove` / `-list`, `org-skim-goto-bookmark`). Depends only on `org-skim-helpers`.

The dependency graph is acyclic: both feature files require `org-skim-helpers`; bookmarks never requires `org-skim`.

## Sanity-check commands

```sh
emacs -Q --batch -L . -f batch-byte-compile org-skim-helpers.el org-skim.el org-skim-bookmarks.el   # catches syntax + docstring lint errors; produces .elc files (delete after)
emacs -Q --batch -L . -l org-skim.el                                                                # load-time smoke test (transitively loads helpers and bookmarks)
```

There are no automated tests. Manual verification requires macOS with Skim and a PDF open.

## Architecture

The package layers three concerns:

1. **Customization + dispatch (`org-skim-helpers.el`)** — the `org-skim` customization group, `org-skim-debug`, `org-skim-applescript-backend`, and `org-skim-toggle-debug` live here, as does the single AppleScript entry point `org-skim--run-applescript SCRIPT &rest ARGV`. It resolves the backend via `org-skim--resolved-backend` and:
   - For `do-applescript`: wraps SCRIPT with a synthesized `run {...}` call (`org-skim--applescript-with-argv`) so scripts that use `on run argv` work identically through both backends.
   - For `osascript`: shells out via `call-process-region`, feeding the script on stdin (`-`) and passing ARGV as positional args.

   Shared utilities also live here: `org-skim--applescript-quote`, `org-skim--front-document-info`, `org-skim--expand-template`.

   All AppleScript-driven functions must go through `org-skim--run-applescript`. Do not call `do-applescript` or `osascript` directly anywhere else — that would bypass the backend abstraction.

2. **Feature functions (`org-skim.el`)** — TOC extraction (`org-skim--toc-string` → `org-skim-insert-toc` / `org-skim-yank-toc`), page navigation (`org-skim-current-page`, `org-skim-next-page`, `org-skim-previous-page`), page links (`org-skim-insert-page-link`, `org-skim-yank-page-link`), and reading bar control (`org-skim-show-reading-bar` and friends). Each is a thin Elisp wrapper around an AppleScript string passed to `org-skim--run-applescript`.

3. **Bookmark functions (`org-skim-bookmarks.el`)** — folder-aware bookmark management (`org-skim-bookmark-add`, `-remove`, `-list`, `org-skim-goto-bookmark`). Uses the helpers for all Skim communication; introduces no new dispatch path.

The TOC AppleScript (`org-skim--toc-applescript`) is the only large embedded script and uses `on run argv` to receive the header character and base level; the navigation/bookmark scripts are inlined as format strings.

## Conventions specific to this package

- Public functions are `org-skim-*`; private helpers and the embedded AppleScript constant are `org-skim--*`. Maintain this split.
- Interactive commands carry `;;;###autoload`. `org-skim-current-page` is intentionally non-interactive (returns a value).
- AppleScript strings begin every Skim interaction with `if (count of documents) is 0 then error "No document open in Skim."` so the Elisp side surfaces a useful error rather than an opaque AppleScript failure.
- Page navigation is bounded inside AppleScript (`if n < (count of pages)` / `if n > 1`) — do not move the bounds check to Elisp, since it would require an extra round trip.
- When extending: prefer reusing `org-skim--run-applescript` over adding a new dispatch path. If a new script needs arguments, use `on run argv` so it works through both backends.
- Keep the dependency direction one-way: feature files (`org-skim.el`, `org-skim-bookmarks.el`) depend on `org-skim-helpers.el`, never on each other. New shared utilities go in helpers.
