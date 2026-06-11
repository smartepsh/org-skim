;;; org-skim-note.el --- Skim note commands for org-skim -*- lexical-binding: t; -*-

;; Author: smartepsh
;; Keywords: pdf, skim, org, notes
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Note (annotation) helpers for Skim, layered on top of
;; `org-skim-helpers'.
;;
;;   `org-skim-get-active-note'  return a plist describing the currently
;;                               selected note in the front Skim document,
;;                               or nil if no note is selected.
;;
;; The plist keys are `:type', `:page', `:text', and `:bounds'.
;; `:type' is the note type string (e.g. \"note\", \"underline note\",
;; \"highlight note\", \"strike out note\").
;; `:page' is a 1-based integer.
;; `:text' is the note text (for markup notes, this is the PDF text).
;; `:bounds' is a list of four floats (left, top, right, bottom).

;;; Code:

(require 'org-skim-helpers)

(defconst org-skim--note-field-separator "\x1f"
  "Delimiter used between fields in the active-note AppleScript reply.")

(defconst org-skim--active-note-applescript
  "tell application \"Skim\"
        set d to front document
        set n to active note of d
        if n is missing value then return \"\"
        set sep to (character id 31)
        set theType to (type of n) as text
        set thePage to (index of (page of n)) as text
        set theText to (text of n) as text
        set {l, t, r, b} to bounds of n
        set theBounds to (l as text) & \",\" & (t as text) & \",\" & (r as text) & \",\" & (b as text)
        return theType & sep & thePage & sep & theText & sep & theBounds
end tell"
  "AppleScript returning the selected note's fields separated by US (0x1F).
Returns an empty string when no note is selected.")

;;;###autoload
(defun org-skim-get-active-note ()
  "Return a plist describing the currently selected note in Skim, or nil.

The plist has the keys `:type', `:page', `:text', and `:bounds'.
`:page' is a 1-based integer (or nil if unavailable); `:type' and
`:text' are strings.  `:bounds' is a list of four floats (left,
top, right, bottom) or nil.  Returns nil when no note is selected
in the front document."
  (org-skim--ensure-document)
  (let ((raw (org-skim--run-applescript org-skim--active-note-applescript)))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (let* ((parts (split-string raw org-skim--note-field-separator))
             (type (nth 0 parts))
             (page-str (nth 1 parts))
             (text (nth 2 parts))
             (bounds-str (nth 3 parts)))
        (list :type type
              :page (and page-str
                         (not (string-empty-p page-str))
                         (string-to-number page-str))
              :text text
              :bounds (and bounds-str
                           (not (string-empty-p bounds-str))
                           (mapcar #'string-to-number
                                   (split-string bounds-str ","))))))))

(defconst org-skim--page1-notes-applescript
  "tell application \"Skim\"
        set d to front document
        set p to page 1 of d
        set noteList to every note of p
        if (count of noteList) is 0 then return \"\"
        set sep to (character id 30)
        set out to \"\"
        repeat with n in noteList
                set out to out & (text of n) & sep
        end repeat
        return out
end tell"
  "AppleScript returning text of every note on page 1, separated by RS (0x1E).
Returns an empty string when page 1 has no notes.")

(defconst org-skim--bibtex-key-regexp ":SKIM:BIBTEX_KEY:\\([^:]+\\):"
  "Regexp matching a Skim-embedded BibTeX key in note text.
The key is captured in group 1.")

;;;###autoload
(defun org-skim-bibtex-key ()
  "Return the BibTeX key embedded in a page-1 note, or nil.

Looks for the pattern \":SKIM:BIBTEX_KEY:KEY:\" in every note on
page 1 of the front Skim document and returns KEY as a string.
If no matching note is found, returns nil."
  (org-skim--ensure-document)
  (let ((raw (org-skim--run-applescript org-skim--page1-notes-applescript)))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (catch 'found
        (dolist (text (split-string raw "\x1e" t))
          (when (string-match org-skim--bibtex-key-regexp text)
            (throw 'found (match-string 1 text))))))))

(defconst org-skim--org-id-regexp ":SKIM:ORG_ID:\\([^:]+\\):"
  "Regexp matching a Skim-embedded Org ID in note text.
The ID is captured in group 1.")

;;;###autoload
(defun org-skim-get-org-id ()
  "Return the Org ID embedded in the active note, or signal an error.

Looks for the pattern \":SKIM:ORG_ID:ID:\" in the text of the currently
selected note in Skim and returns ID as a string.  Signals a `user-error'
if no note is selected."
  (org-skim--ensure-document)
  (let ((note (org-skim-get-active-note)))
    (unless note
      (user-error "No note selected in Skim"))
    (let ((text (plist-get note :text)))
      (if (string-match org-skim--org-id-regexp text)
          (match-string 1 text)
        (user-error "No Org ID found in the active note")))))

(defcustom org-skim-open-org-note-function 'org-id-find
  "Function called to open an Org note by ID.
Receives the ID string as its sole argument.  The default,
`org-id-find', navigates to the heading with that ID."
  :type '(choice (function :tag "Function")
                 (const :tag "org-id-find" org-id-find))
  :group 'org-skim)

;;;###autoload
(defun org-skim-open-org-note ()
  "Open the Org note whose ID is embedded in the active Skim note.
Calls `org-skim-open-org-note-function' with the ID extracted from
the pattern \":SKIM:ORG_ID:ID:\" in the active note.  Signals a
`user-error' if no note is selected or no Org ID is found."
  (interactive)
  (let ((id (org-skim-get-org-id)))
    (funcall org-skim-open-org-note-function id)))

(defcustom org-skim-note-icon-size 16
  "Size in points of the anchored note icon for BibTeX keys.
Used to calculate the note bounds on page 1."
  :type 'integer
  :group 'org-skim)

(defun org-skim--applescript-escape-string (s)
  "Return S with backslashes and double-quotes escaped for inline AppleScript use.
Unlike `org-skim--applescript-quote', this does NOT wrap the result
in double-quotes — it is meant for embedding inside an existing
AppleScript string literal."
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "\\\\" "\\\\\\\\" (or s ""))))

;;;###autoload
(defun org-skim-set-bibtex-key (key)
  "Set the BibTeX key in the front Skim document to KEY.

If a BibTeX key note already exists on page 1 and its key matches
KEY, do nothing.  If the key differs, update the note text in place.
If no such note exists, create a new one with the key icon."
  (org-skim--ensure-document)
  (let* ((escaped-key (org-skim--applescript-escape-string key))
         (icon-size (number-to-string org-skim-note-icon-size)))
    (org-skim--run-applescript
     (concat "tell application \"Skim\"
        set d to front document
        set p to page 1 of d
        set noteList to every note of p
        set found to false
        repeat with n in noteList
                if (text of n) starts with \":SKIM:BIBTEX_KEY:\" then
                        set found to true
                        if (text of n) is not \":SKIM:BIBTEX_KEY:" escaped-key ":\" then
                                set text of n to \":SKIM:BIBTEX_KEY:" escaped-key ":\"
                        end if
                        exit repeat
                end if
        end repeat
        if not found then
                set {l, b, r, t} to bounds of p
                set n to make new note at p with properties {type:anchored note, text:\":SKIM:BIBTEX_KEY:" escaped-key ":\", bounds:{0, b, " icon-size ", b - " icon-size "}}
                set icon of n to key icon
        end if
end tell"))
    (org-skim-save-document)
    key))

(provide 'org-skim-note)

;;; org-skim-note.el ends here
