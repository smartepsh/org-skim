;;; org-skim.el --- Integrate Skim PDF reader with Org mode -*- lexical-binding: t; -*-

;; Author: smartepsh
;; Keywords: pdf, skim, org, outlines
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; org-skim integrates the Skim PDF reader (a macOS app) with Org mode by
;; driving Skim through AppleScript.
;;
;; This file currently provides commands to extract the table of contents
;; (outline) of the PDF open in the front Skim document and turn it into an
;; Org heading tree:
;;
;;   `org-skim-insert-toc' inserts the TOC at point in the current buffer.
;;
;;   `org-skim-yank-toc' copies the TOC to the kill ring instead, so it can
;;   be pasted elsewhere.

;;; Code:

(defgroup org-skim nil
  "Integrate the Skim PDF reader with Org mode."
  :group 'org
  :prefix "org-skim-")

(defcustom org-skim-toc-header-char ?*
  "Character used to build the prefix for each TOC heading level.
The depth of an outline item is rendered by repeating this character,
e.g. with the default `?*' a top-level entry becomes \"* Title\" and a
second-level entry becomes \"** Title\"."
  :type 'character
  :group 'org-skim)

(defcustom org-skim-toc-header-name "TOC"
  "Name of the root heading inserted above the extracted TOC.
A single heading made from one `org-skim-toc-header-char' and this name
is placed above the outline, and every extracted item is shifted down one
level beneath it.  For example, with the defaults the result begins with
\"* TOC\" and a top-level outline entry becomes \"** Title\".

If set to nil or an empty string, no root heading is inserted and the
outline items start at the top level."
  :type '(choice (const :tag "No root heading" nil) string)
  :group 'org-skim)

;;; AppleScript

(defconst org-skim--toc-applescript "\
on outlineToText(theOutlines, theLevel, headerChar)
	set theText to \"\"
	set thePrefix to \"\"
	repeat theLevel times
		set thePrefix to thePrefix & headerChar
	end repeat
	tell application \"Skim\"
		set theCount to count of theOutlines
		repeat with i from 1 to theCount
			set anOutline to item i of theOutlines
			set theName to name of anOutline
			set theText to theText & thePrefix & \" \" & theName & linefeed
			set theChildren to outlines of anOutline
			if (count of theChildren) > 0 then
				set theText to theText & my outlineToText(theChildren, theLevel + 1, headerChar)
			end if
		end repeat
	end tell
	return theText
end outlineToText

on run argv
	set headerChar to item 1 of argv
	set baseLevel to (item 2 of argv) as integer
	tell application \"Skim\"
		if (count of documents) is 0 then error \"No document open in Skim.\"
		set theDoc to front document
		set theOutlines to outlines of theDoc
	end tell
	if (count of theOutlines) is 0 then error \"The front Skim document has no table of contents.\"
	return my outlineToText(theOutlines, baseLevel, headerChar)
end run
"
  "AppleScript that recursively renders the front Skim document's TOC.
It takes two arguments, the header character and the starting depth, and
returns the outline as plain text with each item prefixed by the header
character repeated according to its depth.")

(defun org-skim--toc-string ()
  "Return the front Skim document's table of contents as Org heading text.
When `org-skim-toc-header-name' is a non-empty string, a root heading
\(e.g. \"* TOC\") is placed above the outline and every extracted item is
shifted down one level beneath it.  The string ends with a trailing
newline.  The Skim link/target for each heading is not yet implemented
and is therefore left blank."
  (let* ((has-root (and (stringp org-skim-toc-header-name)
                        (not (string-empty-p org-skim-toc-header-name))))
         (base-level (if has-root 2 1))
         (body (with-temp-buffer
                 ;; Feed the script to osascript on stdin (the "-" argument)
                 ;; and let it replace the buffer with the script's output.
                 (insert org-skim--toc-applescript)
                 (let ((status (call-process-region
                                (point-min) (point-max)
                                "osascript" t t nil
                                "-"
                                (char-to-string org-skim-toc-header-char)
                                (number-to-string base-level))))
                   (unless (eq status 0)
                     (error "org-skim: %s" (string-trim (buffer-string))))
                   (buffer-string)))))
    (if has-root
        (concat (char-to-string org-skim-toc-header-char) " "
                org-skim-toc-header-name "\n" body)
      body)))

;;; Commands

;;;###autoload
(defun org-skim-insert-toc ()
  "Insert the table of contents of the front Skim document at point.
The outline is rendered as a tree of Org headings, one per line, with the
depth indicated by repeating `org-skim-toc-header-char'.

The TOC is inserted into the current buffer regardless of its major mode.
To copy it to the kill ring instead, use `org-skim-yank-toc'.

The Skim link/target for each heading is not yet implemented."
  (interactive)
  (insert (org-skim--toc-string)))

;;;###autoload
(defun org-skim-yank-toc ()
  "Copy the table of contents of the front Skim document to the kill ring.
Unlike `org-skim-insert-toc', this never inserts into the current
buffer; it only yanks the TOC so it can be pasted elsewhere."
  (interactive)
  (kill-new (org-skim--toc-string))
  (message "org-skim: table of contents copied to kill ring"))

(provide 'org-skim)

;;; org-skim.el ends here
