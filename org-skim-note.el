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
;; The plist keys are `:type', `:page', `:text', `:org-id', and `:bounds'.
;; `:type' is the note type string (e.g. \"note\", \"underline note\",
;; \"highlight note\", \"strike out note\").
;; `:page' is a 1-based integer.
;; `:text' is the note text (for markup notes, this is the PDF text),
;; with any embedded \":SKIM:ORG_ID:...:\" marker removed.
;; `:org-id' is the embedded Org ID string, or nil.
;; `:bounds' is a list of four floats (left, top, right, bottom).
;;
;;   `org-skim-capture-note'     org-capture an Org heading for the active
;;                               Skim note, embedding a shared Org ID.

;;; Code:

(require 'subr-x)
(require 'org-skim-helpers)

(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-capture-get "org-capture" (prop &optional local))
(declare-function org-capture-target-buffer "org-capture" (file))
(declare-function org-id-new "org-id" (&optional prefix))
(declare-function org-id-add-location "org-id" (id file))
(declare-function org-id-find "org-id" (id &optional markerp))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function citar-get-value "citar" (field key-or-entry))
(declare-function citar-get-files "citar" (&optional key-or-keys))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(defvar org-note-abort)
(defvar org-capture-templates)
(defvar citar-notes-paths)

(defconst org-skim--note-field-separator "\x1f"
  "Delimiter used between fields in the active-note AppleScript reply.")

(defconst org-skim--org-id-regexp ":SKIM:ORG_ID:\\([^:]+\\):"
  "Regexp matching a Skim-embedded Org ID in note text.
The ID is captured in group 1.")

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

The plist has the keys `:type', `:page', `:text', `:org-id', and
`:bounds'.  `:page' is a 1-based integer (or nil if unavailable);
`:type' and `:text' are strings.  `:text' has any embedded
\":SKIM:ORG_ID:...:\" marker removed; the ID itself is returned
under `:org-id' (or nil if absent).  `:bounds' is a list of four
floats (left, top, right, bottom) or nil.  Returns nil when no
note is selected in the front document."
  (org-skim--ensure-document)
  (let ((raw (org-skim--run-applescript org-skim--active-note-applescript)))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (let* ((parts (split-string raw org-skim--note-field-separator))
             (type (nth 0 parts))
             (page-str (nth 1 parts))
             (raw-text (nth 2 parts))
             (org-id (and (string-match org-skim--org-id-regexp raw-text)
                          (match-string 1 raw-text)))
             (text (string-trim
                    (replace-regexp-in-string
                     (concat org-skim--org-id-regexp "\n?") ""
                     raw-text)))
             (bounds-str (nth 3 parts)))
        (list :type type
              :page (and page-str
                         (not (string-empty-p page-str))
                         (string-to-number page-str))
              :text text
              :org-id org-id
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

;;;###autoload
(defun org-skim-get-org-id ()
  "Return the Org ID embedded in the active note, or signal an error.

Looks for the pattern \":SKIM:ORG_ID:ID:\" in the text of the currently
selected note in Skim and returns ID as a string.  Signals a `user-error'
if no note is selected."
  (let ((note (org-skim-get-active-note)))
    (unless note
      (user-error "No note selected in Skim"))
    (or (plist-get note :org-id)
        (user-error "No Org ID found in the active note"))))

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

(defun org-skim--org-note-content (id)
  "Return the body of the Org entry with ID as a string, or nil.
Returns nil when ID does not resolve to an existing Org entry.
The heading line, planning lines, and drawers (PROPERTIES included)
are stripped; only the entry's content up to the end of its subtree
is returned, trimmed of surrounding whitespace."
  (require 'org-id)
  (let ((location (org-id-find id)))
    (when location
      (with-current-buffer (find-file-noselect (car location))
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (cdr location))
            (let ((end (save-excursion (org-end-of-subtree t t) (point))))
              (org-end-of-meta-data t)
              (string-trim
               (buffer-substring-no-properties (min (point) end) end)))))))))

;;;###autoload
(defun org-skim-read-org-note ()
  "Return the content of the Org note linked to the active Skim note.
The note's embedded Org ID (\":SKIM:ORG_ID:ID:\") is resolved with
`org-id-find' and the entry's body (heading and drawers stripped,
see `org-skim--org-note-content') is returned as a string; no
buffer is displayed.  Signals a `user-error' if no note is
selected, no Org ID is embedded, or the ID does not resolve."
  (interactive)
  (let ((id (org-skim-get-org-id)))
    (or (org-skim--org-note-content id)
        (user-error "Org ID %s does not resolve to an Org note" id))))

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

(defcustom org-skim-note-file-function #'org-skim--citar-note-file
  "Function mapping a BibTeX key to the Org notes file path.
Receives the citekey string and returns an absolute file name.
The default resolves through citar's file-per-key notes layout."
  :type 'function
  :group 'org-skim)

(defcustom org-skim-capture-template
  "* %(org-skim-capture-value :heading)
:PROPERTIES:
:ID:        %(org-skim-capture-value :id)
:REFERENCES: @%(org-skim-capture-value :citekey)
:SKIM_PAGE: %(org-skim-capture-value :page)
:END:
%?"
  "Org capture template used by `org-skim-capture-note'.
Use `%(org-skim-capture-value KEY)' to splice in fields of the
pending Skim note: `:id', `:citekey', `:page', `:text' (the full
note text), or `:heading' (the note text collapsed to one line)."
  :type 'string
  :group 'org-skim)

(defcustom org-skim-note-title-template "Notes on ${title}/${year}"
  "Template for the #+title of a newly created notes file.
${...} variables are resolved from the citekey's bibliography
entry via citar: `title', `author', `year', and `citekey'.
Missing fields expand to the citekey (for `title') or an empty
string; unknown variables are left in place."
  :type 'string
  :group 'org-skim)

(defcustom org-skim-note-filetags "ReadingNote"
  "Tag(s) for the #+filetags keyword of a newly created notes file.
Surrounding colons are added automatically; use \"a:b\" for
multiple tags.  Set to nil to omit the #+filetags line."
  :type '(choice (const :tag "None" nil) string)
  :group 'org-skim)

(defcustom org-skim-note-references-property "REFERENCES"
  "File-level property naming the @citekey reference in new notes files.
Set to nil to omit the property.  Note that the property used in
captured headings is spelled out in `org-skim-capture-template'."
  :type '(choice (const :tag "None" nil) string)
  :group 'org-skim)

(defvar org-skim--pending-capture nil
  "Plist describing the Skim note currently being captured.
Keys are `:id', `:citekey', `:page', `:text', and `:heading'.
Set by `org-skim-capture-note' and cleared when the capture
finalizes or aborts.")

(defun org-skim-capture-value (key)
  "Return field KEY of the pending Skim capture as a string.
Intended for `%(...)' escapes in `org-skim-capture-template'."
  (let ((value (plist-get org-skim--pending-capture key)))
    (cond ((numberp value) (number-to-string value))
          ((stringp value) value)
          (t ""))))

(defun org-skim--single-line (text)
  "Return TEXT with line breaks collapsed to single spaces."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " (or text ""))))

(defun org-skim--citar-note-file (citekey)
  "Resolve CITEKEY to its notes file inside `citar-notes-paths'.
The file is named after the title expanded from
`org-skim-note-title-template', with \"/\" replaced by \"-\"."
  (unless (require 'citar nil t)
    (user-error "Citar is not installed; customize `org-skim-note-file-function'"))
  (expand-file-name
   (concat (replace-regexp-in-string "/" "-" (org-skim--note-title citekey))
           ".org")
   (car citar-notes-paths)))

(defun org-skim--citar-field (field citekey)
  "Return FIELD of CITEKEY's bibliography entry via citar, or nil."
  (and (require 'citar nil t)
       (ignore-errors (citar-get-value field citekey))))

(defun org-skim--note-expand (template vars)
  "Expand ${...} variables in TEMPLATE against the string alist VARS.
Unknown variables are left in place."
  (replace-regexp-in-string
   "\\${\\([^}]+\\)}"
   (lambda (m)
     (or (cdr (assoc (match-string 1 m) vars)) m))
   template t t))

(defun org-skim--note-title (citekey)
  "Return the note title for CITEKEY per `org-skim-note-title-template'.
${...} variables are resolved from the citekey's bibliography
entry via citar."
  (let* ((title (or (org-skim--citar-field "title" citekey) citekey))
         (year (or (org-skim--citar-field "year" citekey)
                   (let ((date (org-skim--citar-field "date" citekey)))
                     (and date (>= (length date) 4) (substring date 0 4)))
                   ""))
         (author (or (org-skim--citar-field "author" citekey) ""))
         (vars (list (cons "title" title)
                     (cons "year" year)
                     (cons "author" author)
                     (cons "citekey" citekey))))
    (org-skim--note-expand org-skim-note-title-template vars)))

(defun org-skim--new-note-frontmatter (citekey)
  "Return the file-level frontmatter for a new notes file for CITEKEY.
Comprises a property drawer with a fresh Org ID and the
`org-skim-note-references-property', a #+title expanded from
`org-skim-note-title-template', #+filetags from
`org-skim-note-filetags', and a #+created date stamp."
  (concat
   ":PROPERTIES:\n"
   ":ID:       " (org-id-new) "\n"
   (and org-skim-note-references-property
        (format ":%s: @%s\n" org-skim-note-references-property citekey))
   ":END:\n"
   "#+title: " (org-skim--note-title citekey) "\n"
   (and org-skim-note-filetags
        (format "#+filetags: :%s:\n"
                (string-trim org-skim-note-filetags ":+" ":+")))
   "#+created: " (format-time-string "[%Y-%m-%d]") "\n\n"))

(defun org-skim--capture-target ()
  "Position point for capturing the pending Skim note.
Opens the notes file for the pending citekey, inserting the
file-level frontmatter when the file is new, and moves to its end."
  (let* ((citekey (plist-get org-skim--pending-capture :citekey))
         (file (funcall org-skim-note-file-function citekey)))
    (set-buffer (org-capture-target-buffer file))
    (when (= (buffer-size) 0)
      (insert (org-skim--new-note-frontmatter citekey)))
    (goto-char (point-max))))

(defun org-skim--capture-after-finalize ()
  "Register the captured heading's Org ID; clear the pending capture.
No-op for captures not started by `org-skim-capture-note'."
  (when org-skim--pending-capture
    (unless org-note-abort
      (let ((file (buffer-file-name (org-capture-get :buffer))))
        (when file
          (org-id-add-location (plist-get org-skim--pending-capture :id)
                               file))))
    (setq org-skim--pending-capture nil)))

(add-hook 'org-capture-after-finalize-hook #'org-skim--capture-after-finalize)

(defconst org-skim--embed-org-id-applescript
  "on run argv
	set theLine to item 1 of argv
	tell application \"Skim\"
		set n to active note of front document
		if n is missing value then error \"No note selected in Skim.\"
		set text of n to theLine & linefeed & (text of n)
	end tell
end run"
  "AppleScript prepending its first argument as a line to the active note.")

(defun org-skim--embed-org-id (id)
  "Prepend the \":SKIM:ORG_ID:ID:\" marker line to the active Skim note.
Saves the document and returns ID."
  (org-skim--ensure-document)
  (org-skim--run-applescript org-skim--embed-org-id-applescript
                             (format ":SKIM:ORG_ID:%s:" id))
  (org-skim-save-document)
  id)

;;;###autoload
(defun org-skim-capture-note ()
  "Capture an Org heading for the active note in Skim.

Generates a fresh Org ID (reusing one already embedded in the
note, if any), prepends the \":SKIM:ORG_ID:...:\" marker line to
the Skim note text, then starts an org-capture into the notes
file for the document's BibTeX key (see
`org-skim-note-file-function' and `org-skim-capture-template').
On finalize, the ID is registered with `org-id-add-location' so
`org-skim-open-org-note' can resolve it."
  (interactive)
  (require 'org-id)
  (require 'org-capture)
  (org-skim--ensure-document)
  (let* ((note (or (org-skim-get-active-note)
                   (user-error "No note selected in Skim")))
         (citekey (or (org-skim-bibtex-key)
                      (user-error "No BibTeX key note on page 1 in Skim")))
         (id (or (plist-get note :org-id)
                 (org-skim--embed-org-id (org-id-new))))
         (text (plist-get note :text)))
    (setq org-skim--pending-capture
          (list :id id
                :citekey citekey
                :page (plist-get note :page)
                :text text
                :heading (org-skim--single-line text)))
    (condition-case err
        (let ((org-capture-templates
               `(("s" "Skim note" entry (function org-skim--capture-target)
                  ,org-skim-capture-template :empty-lines-before 1))))
          (org-capture nil "s"))
      ((error quit)
       (setq org-skim--pending-capture nil)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun org-skim-note-dwim ()
  "Capture or read the Org note for the active note in Skim.

If the active note has no embedded \":SKIM:ORG_ID:...:\" marker, or
the embedded ID does not resolve to an existing Org entry, start
`org-skim-capture-note' (which reuses an already embedded ID).
Otherwise return the linked entry's content as a string, as with
`org-skim-read-org-note'; no buffer is displayed.  Intended as the
single key binding for the Skim note workflow."
  (interactive)
  (require 'org-id)
  (let* ((note (or (org-skim-get-active-note)
                   (user-error "No note selected in Skim")))
         (id (plist-get note :org-id))
         (content (and id (org-skim--org-note-content id))))
    (or content (org-skim-capture-note))))

(defcustom org-skim-open-note-extensions '("pdf")
  "File extensions `org-skim-open-note-in-skim' will open in Skim.
Each is a lowercase extension without a leading dot.  When the
heading's REFERENCES citekey resolves to several files, the first
one whose extension is a member of this list is opened.  Only
\"pdf\" is supported for now; the list shape lets it grow later."
  :type '(repeat string)
  :group 'org-skim)

(defun org-skim--skim-note-at-point ()
  "Return a plist describing the Skim note for the Org heading at point, or nil.

The plist has keys `:page' (1-based integer), `:id' (the Org ID
string), and `:citekey' (the REFERENCES citekey with any leading
\"@\" stripped).  `SKIM_PAGE' and `ID' are read from the heading
itself; `REFERENCES' is read with inheritance, so it resolves from
the file-level property drawer no matter how deeply the heading is
nested.  Returns nil when any of the three is absent."
  (require 'org)
  (let* ((page (org-entry-get (point) "SKIM_PAGE"))
         (id (org-entry-get (point) "ID"))
         (references (org-entry-get (point) "REFERENCES" t)))
    (when (and page (not (string-empty-p page))
               id (not (string-empty-p id))
               references (not (string-empty-p references)))
      (list :page (string-to-number page)
            :id id
            :citekey (string-remove-prefix "@" (string-trim references))))))

(defun org-skim--citar-file-for-citekey (citekey)
  "Return the first file of CITEKEY whose extension is allowed, or nil.
Allowed extensions are `org-skim-open-note-extensions', matched
case-insensitively.  Resolves files via citar; signals a
`user-error' when citar is not installed."
  (unless (require 'citar nil t)
    (user-error "Citar is not installed; cannot resolve citekey %s" citekey))
  (let ((files (gethash citekey (citar-get-files citekey))))
    (seq-find
     (lambda (file)
       (member (downcase (or (file-name-extension file) ""))
               org-skim-open-note-extensions))
     files)))

(defconst org-skim--open-note-applescript
  "on run argv
	set thePath to item 1 of argv
	set thePage to (item 2 of argv) as integer
	set theId to item 3 of argv
	set marker to \":SKIM:ORG_ID:\" & theId & \":\"
	tell application \"Skim\"
		set theAlias to POSIX file thePath as alias
		open theAlias
		set targetDoc to missing value
		repeat with i from 1 to (count of documents)
			set d to document i
			try
				set dp to POSIX path of (file of d as alias)
				if dp is thePath then
					set targetDoc to d
					exit repeat
				end if
			end try
		end repeat
		if targetDoc is missing value then set targetDoc to front document
		set thePageRef to page thePage of targetDoc
		go targetDoc to thePageRef
		repeat with n in (every note of thePageRef)
			if (text of n) contains marker then
				set active note of targetDoc to n
				exit repeat
			end if
		end repeat
	end tell
end run"
  "AppleScript that opens a PDF and selects the note carrying an Org ID.
Arguments: POSIX path, page index, and Org ID.  Opens the file,
finds the matching open document, goes to the page, then sets as
the active note the first note on that page whose text contains
\":SKIM:ORG_ID:<id>:\".  Uses no `activate', so Emacs keeps focus.")

;;;###autoload
(defun org-skim-open-note-in-skim ()
  "Open the Skim note for the Org heading at point in Skim.

The heading must carry `SKIM_PAGE' and `ID' properties and inherit
a `REFERENCES' citekey (typically from the file-level property
drawer).  The citekey is resolved to a file via citar; the first
file whose extension is in `org-skim-open-note-extensions' is
opened in Skim, navigated to `SKIM_PAGE', and the note bearing this
heading's Org ID is selected.  Focus stays on Emacs.

When the heading is not a Skim note (any of the three properties
missing), print a message and do nothing."
  (interactive)
  (let ((note (org-skim--skim-note-at-point)))
    (if (null note)
        (message "Not a Skim note: heading needs SKIM_PAGE, ID, and a REFERENCES citekey")
      (let* ((citekey (plist-get note :citekey))
             (file (or (org-skim--citar-file-for-citekey citekey)
                       (user-error "No %s file for citekey %s"
                                   (string-join org-skim-open-note-extensions "/")
                                   citekey))))
        (org-skim--run-applescript org-skim--open-note-applescript
                                   (expand-file-name file)
                                   (number-to-string (plist-get note :page))
                                   (plist-get note :id))))))

(provide 'org-skim-note)

;;; org-skim-note.el ends here
