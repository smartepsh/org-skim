;;; org-skim-helpers.el --- Shared helpers for org-skim -*- lexical-binding: t; -*-

;; Author: smartepsh
;; Keywords: pdf, skim, org
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Shared infrastructure used by `org-skim' and its extension modules:
;;
;;   - the `org-skim' customization group;
;;   - the AppleScript dispatch layer (`org-skim--run-applescript' and its
;;     backend selection);
;;   - small utilities for quoting strings and reading information about
;;     the front Skim document.
;;
;; This file is loaded by `org-skim' and by every extension that needs to
;; talk to Skim.  It must not require any other `org-skim-*' file, so the
;; dependency graph stays acyclic.

;;; Code:

(require 'cl-lib)

(defgroup org-skim nil
  "Integrate the Skim PDF reader with Org mode."
  :group 'org
  :prefix "org-skim-")

(defcustom org-skim-debug nil
  "When non-nil, surface AppleScript error messages from Skim.
Errors are otherwise swallowed with a terse \"AppleScript error 1\"
because the `do-applescript' bridge collapses AppleScript's error
strings at the C level before Emacs sees them, and that cannot be
recovered with a `try'/`on error' wrapper.  Turning this on therefore
forces the `osascript' subprocess backend (which writes the full error
text to stderr) regardless of `org-skim-applescript-backend', and any
error that occurs is logged to *Messages* instead of being signalled."
  :type 'boolean
  :group 'org-skim)

(defcustom org-skim-applescript-backend 'auto
  "Backend used to execute AppleScript.
`do-applescript' calls the built-in OSA bridge (faster, but only
available when Emacs is built with macOS GUI support).  `osascript'
shells out to the `osascript' binary (works in any macOS Emacs,
including a terminal build).  `auto' picks `do-applescript' when
available and falls back to `osascript' otherwise."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "do-applescript (built-in)" do-applescript)
                 (const :tag "osascript (subprocess)" osascript))
  :group 'org-skim)

;;;###autoload
(defun org-skim-toggle-debug ()
  "Toggle `org-skim-debug' and report the new state."
  (interactive)
  (setq org-skim-debug (not org-skim-debug))
  (message "org-skim debug %s" (if org-skim-debug "enabled" "disabled")))

(defun org-skim--resolved-backend ()
  "Return the concrete backend symbol implied by `org-skim-applescript-backend'.
When `org-skim-debug' is non-nil, force the `osascript' backend so that
AppleScript error messages reach Emacs; `do-applescript' collapses them
to a bare \"AppleScript error 1\"."
  (cond
   (org-skim-debug 'osascript)
   ((eq org-skim-applescript-backend 'auto)
    (if (fboundp 'do-applescript) 'do-applescript 'osascript))
   (t org-skim-applescript-backend)))

(defun org-skim--applescript-argv-call (argv)
  "Return the AppleScript expression `run {...}' that invokes `on run' with ARGV.
ARGV is a list of strings; each is quoted and escaped for AppleScript."
  (let ((quoted (mapconcat
                 (lambda (s)
                   (concat "\""
                           (replace-regexp-in-string
                            "\"" "\\\\\""
                            (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
                           "\""))
                 argv ", ")))
    (concat "run {" quoted "}")))

(defun org-skim--applescript-with-argv (script argv)
  "Wrap SCRIPT so its `on run argv' handler sees ARGV under `do-applescript'.
ARGV is a list of strings.  Strings are quoted and escaped for AppleScript."
  (concat script "\nreturn " (org-skim--applescript-argv-call argv) "\n"))

(defun org-skim--run-applescript (script &rest argv)
  "Run AppleScript SCRIPT and return its trimmed output as a string.
ARGV are passed as the `on run argv' arguments.  The backend is chosen
according to `org-skim-applescript-backend'.  When `org-skim-debug' is
non-nil, errors are logged to *Messages* instead of being signalled."
  (condition-case err
      (org-skim--applescript-dispatch script argv)
    (error
     (if org-skim-debug
         (progn (message "%s" (error-message-string err)) nil)
       (signal (car err) (cdr err))))))

(defun org-skim--applescript-dispatch (script argv)
  "Dispatch SCRIPT with ARGV to the resolved AppleScript backend."
  (pcase (org-skim--resolved-backend)
    ('do-applescript
     (let* ((wrapped (if argv (org-skim--applescript-with-argv script argv) script))
            (result (condition-case err
                        (do-applescript wrapped)
                      (error (error "org-skim: %s" (error-message-string err)))))
            (text (cond ((stringp result) result)
                        ((numberp result) (number-to-string result))
                        (t (format "%s" result)))))
       (string-trim text)))
    ('osascript
     (with-temp-buffer
       (insert script)
       (let ((status (apply #'call-process-region
                            (point-min) (point-max)
                            "osascript" t t nil
                            "-" argv)))
         (unless (eq status 0)
           (error "org-skim: %s" (string-trim (buffer-string))))
         (string-trim (buffer-string)))))
    (other (error "org-skim: unknown applescript backend %S" other))))

(defun org-skim--applescript-quote (s)
  "Return S as an AppleScript string literal, with quotes and backslashes escaped."
  (concat "\""
          (replace-regexp-in-string
           "\"" "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\" (or s "")))
          "\""))

(defun org-skim--front-document-info ()
  "Return an alist of facts about the front Skim document.
Keys: `name' (display name), `path' (POSIX path), `page' (1-based page
index as integer)."
  (let* ((raw (org-skim--run-applescript
               "tell application \"Skim\"
        if (count of documents) is 0 then error \"No document open in Skim.\"
        set d to front document
        set thePath to POSIX path of (file of d)
        set theName to name of d
        set thePage to index of current page of d
        return theName & linefeed & thePath & linefeed & (thePage as string)
end tell"))
         (parts (split-string raw "\n")))
    (list (cons 'name (nth 0 parts))
          (cons 'path (nth 1 parts))
          (cons 'page (string-to-number (nth 2 parts))))))

(defun org-skim--expand-template (template info)
  "Expand ${...} variables in TEMPLATE against INFO.
INFO is an alist as returned by `org-skim--front-document-info'.
PAGE may be overridden by INFO's `page' entry.  Unknown variables are
left in place."
  (let* ((name (or (cdr (assq 'name info)) ""))
         (path (or (cdr (assq 'path info)) ""))
         (page (or (cdr (assq 'page info)) 0))
         (file (file-name-base (or path "")))
         (vars `(("title" . ,name)
                 ("file"  . ,file)
                 ("path"  . ,path)
                 ("page"  . ,(number-to-string page))
                 ("date"  . ,(format-time-string "%Y-%m-%d"))
                 ("time"  . ,(format-time-string "%H:%M")))))
    (replace-regexp-in-string
     "\\${\\([^}]+\\)}"
     (lambda (m)
       (let* ((key (match-string 1 m))
              (val (cdr (assoc key vars))))
         (or val m)))
     template t t)))

(provide 'org-skim-helpers)

;;; org-skim-helpers.el ends here
