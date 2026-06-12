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

(require 'org-skim-helpers)

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

(defcustom org-skim-page-link-format "${title} P${page}"
  "Template used to render the description of an inserted page link.
See `org-skim-bookmark-name-format' for the supported ${...} variables."
  :type 'string
  :group 'org-skim)

;;; AppleScript

(defconst org-skim--toc-applescript "\
on outlineToText(theOutlines, theLevel, headerChar, thePath)
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
			try
				set thePage to index of (page of anOutline)
				set theHeading to \"[[skim:\" & thePath & \"::\" & (thePage as string) & \"][\" & theName & \"]]\"
			on error
				set theHeading to theName
			end try
			set theText to theText & thePrefix & \" \" & theHeading & linefeed
			set theChildren to outlines of anOutline
			if (count of theChildren) > 0 then
				set theText to theText & my outlineToText(theChildren, theLevel + 1, headerChar, thePath)
			end if
		end repeat
	end tell
	return theText
end outlineToText

on run argv
	set headerChar to item 1 of argv
	set baseLevel to (item 2 of argv) as integer
	tell application \"Skim\"
		set theDoc to front document
		set thePath to path of theDoc
		set theOutlines to outlines of theDoc
	end tell
	if (count of theOutlines) is 0 then error \"The front Skim document has no table of contents.\"
	return my outlineToText(theOutlines, baseLevel, headerChar, thePath)
end run
"
  "AppleScript that recursively renders the front Skim document's TOC.
It takes two arguments, the header character and the starting depth, and
returns the outline as Org headings.  Each heading is a clickable
`skim:PATH::PAGE' link whose description is the outline item's name and
whose target is the item's page in the front document; an item that
exposes no page falls back to its plain name.  Items are prefixed by the
header character repeated according to depth.")

(defun org-skim--toc-string ()
  "Return the front Skim document's table of contents as Org heading text.
When `org-skim-toc-header-name' is a non-empty string, a root heading
\(e.g. \"* TOC\") is placed above the outline and every extracted item is
shifted down one level beneath it.  Each heading is a clickable
`skim:PATH::PAGE' link to the outline item's page (see
`org-skim-open-page-link'); the optional root heading carries no link."
  (org-skim--ensure-document)
  (let* ((has-root (and (stringp org-skim-toc-header-name)
                        (not (string-empty-p org-skim-toc-header-name))))
         (base-level (if has-root 2 1))
         (body (org-skim--run-applescript
                org-skim--toc-applescript
                (char-to-string org-skim-toc-header-char)
                (number-to-string base-level))))
    (if has-root
        (concat (char-to-string org-skim-toc-header-char) " "
                org-skim-toc-header-name "\n" body "\n")
      (concat body "\n"))))

;;; Page navigation

(defun org-skim-current-page ()
  "Return the 1-based index of the current page in the front Skim document."
  (org-skim--ensure-document)
  (string-to-number
   (org-skim--run-applescript
    "tell application \"Skim\"
        return (index of current page of front document) as string
end tell")))

;;;###autoload
(defun org-skim-next-page ()
  "Go to the next page in the front Skim document."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   "tell application \"Skim\"
        tell front document
                set n to (index of current page)
                if n < (count of pages) then go it to page (n + 1) of it
        end tell
end tell"))

;;;###autoload
(defun org-skim-previous-page ()
  "Go to the previous page in the front Skim document."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   "tell application \"Skim\"
        tell front document
                set n to (index of current page)
                if n > 1 then go it to page (n - 1) of it
        end tell
end tell"))

(defconst org-skim--goto-page-applescript
  "on run argv
	set n to (item 1 of argv) as integer
	tell application \"Skim\"
		tell front document
			set total to count of pages
			if n < 1 then set n to 1
			if n > total then set n to total
			go it to page n of it
		end tell
	end tell
end run"
  "AppleScript jumping the front document to the page in its first argument.
The requested page is clamped to the valid range [1, page count].")

;;;###autoload
(defun org-skim-goto-page (page)
  "Go to PAGE (a 1-based integer) in the front Skim document.
PAGE is clamped to the valid range; values below 1 go to the
first page and values past the last go to the last page.

Interactively, a numeric prefix argument supplies PAGE; with no
prefix, prompt for it in the minibuffer, defaulting to the
current page."
  (interactive
   (list (cond ((integerp current-prefix-arg) current-prefix-arg)
               (current-prefix-arg (prefix-numeric-value current-prefix-arg))
               (t (read-number "Go to page: " (org-skim-current-page))))))
  (org-skim--ensure-document)
  (org-skim--run-applescript org-skim--goto-page-applescript
                             (number-to-string page)))

;;; Page links

(defconst org-skim--open-applescript
  "on run argv
	set thePath to item 1 of argv
	tell application \"Skim\" to open POSIX file thePath
end run"
  "AppleScript opening the file named by its first argument in Skim.
The opened document becomes the front document, so callers may then
drive it through `org-skim--goto-page-applescript'.  Intentionally
omits the \"No document open\" guard: opening a document is how the
front document comes to exist.")

(defun org-skim--page-link-string ()
  "Return a clickable Org link for the current page of the front Skim document.
The target is `skim:PATH::PAGE' (raw absolute path, 1-based page); the
description is rendered with `org-skim-page-link-format'.  Following the
link opens PATH in Skim and goes to PAGE (see `org-skim-open-page-link')."
  (org-skim--ensure-document)
  (let* ((info (org-skim--front-document-info))
         (path (cdr (assq 'path info)))
         (page (cdr (assq 'page info)))
         (desc (org-skim--expand-template org-skim-page-link-format info)))
    (format "[[skim:%s::%d][%s]]" path page desc)))

;;;###autoload
(defun org-skim-insert-page-link ()
  "Insert an Org link to the current page of the front Skim document at point."
  (interactive)
  (insert (org-skim--page-link-string)))

;;;###autoload
(defun org-skim-yank-page-link ()
  "Copy an Org link to the current page of the front Skim document.
The link is placed on the kill ring."
  (interactive)
  (kill-new (org-skim--page-link-string))
  (message "org-skim: page link copied to kill ring"))

(defun org-skim-open-page-link (link)
  "Follow a `skim:PATH::PAGE' LINK: open PATH in Skim and go to PAGE.
LINK is the link body Org passes after stripping the `skim:' type.
A trailing `::PAGE' is matched anchored to the end of LINK so a `::'
appearing earlier in PATH is not mis-split; a link with no trailing
`::PAGE' opens PATH without moving."
  (let* ((sep (string-match "::\\([0-9]+\\)\\'" link))
         (path (if sep (substring link 0 sep) link))
         (page (and sep (string-to-number (match-string 1 link)))))
    (org-skim--run-applescript org-skim--open-applescript path)
    (when page
      (org-skim--run-applescript org-skim--goto-page-applescript
                                 (number-to-string page)))))

(require 'ol)
(org-link-set-parameters "skim" :follow #'org-skim-open-page-link)

;;; Reading bar

;;;###autoload
(defun org-skim-show-reading-bar ()
  "Show the reading bar in the front Skim document on the current page.
If the bar is already visible, this is a no-op."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   "tell application \"Skim\"
        set d to front document
        set hasBar to has reading bar of d
        if hasBar is false then
                set has reading bar of d to true
                go (reading bar of d) to (line 1 of current page of d)
        end if
end tell"))

;;;###autoload
(defun org-skim-hide-reading-bar ()
  "Hide the reading bar in the front Skim document."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   "tell application \"Skim\"
        set d to front document
        if has reading bar of d then set has reading bar of d to false
end tell"))

;;;###autoload
(defun org-skim-toggle-reading-bar ()
  "Toggle the reading bar in the front Skim document.
When turning the bar on, it lands on the first line of the current page."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   "tell application \"Skim\"
        set d to front document
        if has reading bar of d then
                set has reading bar of d to false
        else
                set has reading bar of d to true
                go (reading bar of d) to (line 1 of current page of d)
        end if
end tell"))

(defconst org-skim--reading-bar-move-applescript "\
on run argv
        set direction to item 1 of argv
        tell application \"Skim\"
                set d to front document
                set hasBar to has reading bar of d
                if hasBar is false then
                        set has reading bar of d to true
                        go (reading bar of d) to (line 1 of current page of d)
                        return
                end if
                set rb to reading bar of d
                set lineStep to (width of rb)
                if lineStep < 1 then set lineStep to 1
                set barPage to page of rb
                set barPageIdx to index of barPage
                set barLines to lines of rb
                set firstIdx to index of (item 1 of barLines)
                set lastIdx to index of (item -1 of barLines)
                set linesOnPage to count of (lines of barPage)
                if direction is \"down\" then
                        set targetIdx to lastIdx + 1
                        if targetIdx > linesOnPage then
                                try
                                        set np to page (barPageIdx + 1) of d
                                        go rb to (line 1 of np)
                                on error
                                        go rb to (line linesOnPage of barPage)
                                end try
                        else
                                go rb to (line targetIdx of barPage)
                        end if
                else
                        set targetIdx to firstIdx - lineStep
                        if targetIdx < 1 then
                                try
                                        set pp to page (barPageIdx - 1) of d
                                        set lastOnPrev to count of (lines of pp)
                                        set toIdx to lastOnPrev - lineStep + 1
                                        if toIdx < 1 then set toIdx to 1
                                        go rb to (line toIdx of pp)
                                on error
                                        go rb to (line 1 of barPage)
                                end try
                        else
                                go rb to (line targetIdx of barPage)
                        end if
                end if
        end tell
end run
"
  "AppleScript that advances the front document's reading bar.
Argument: \"down\" or \"up\".  Rolls across page boundaries; if the bar
isn't visible, it is shown on the current page's first line.")

;;;###autoload
(defun org-skim-reading-bar-next-line ()
  "Move the reading bar down one line in the front Skim document.
At the bottom of a page the bar rolls to the next page; at the very
last page it stays on the final line."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   org-skim--reading-bar-move-applescript "down"))

;;;###autoload
(defun org-skim-reading-bar-previous-line ()
  "Move the reading bar up one line in the front Skim document.
At the top of a page the bar rolls to the previous page; at the very
first page it stays on the first line."
  (interactive)
  (org-skim--ensure-document)
  (org-skim--run-applescript
   org-skim--reading-bar-move-applescript "up"))

;;; Commands

;;;###autoload
(defun org-skim-insert-toc ()
  "Insert the table of contents of the front Skim document at point.
The outline is rendered as a tree of Org headings, one per line, with the
depth indicated by repeating `org-skim-toc-header-char'.

The TOC is inserted into the current buffer regardless of its major mode.
To copy it to the kill ring instead, use `org-skim-yank-toc'.

Each heading is a clickable `skim:' link to the outline item's page;
follow it with \\[org-open-at-point] to jump there in Skim."
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

(require 'org-skim-bookmarks)
(require 'org-skim-note)

(provide 'org-skim)

;;; org-skim.el ends here
