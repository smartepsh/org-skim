;;; org-skim-bookmarks.el --- Skim bookmark commands for org-skim -*- lexical-binding: t; -*-

;; Author: smartepsh
;; Keywords: pdf, skim, org, bookmarks
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Bookmark management commands for Skim, layered on top of
;; `org-skim-helpers'.
;;
;;   `org-skim-bookmark-add'     create a bookmark at the current page.
;;   `org-skim-bookmark-remove'  delete a bookmark chosen via completion.
;;   `org-skim-goto-bookmark'    open and jump to a bookmark.
;;   `org-skim-bookmark-list'    list all of Skim's file bookmarks.

;;; Code:

(require 'cl-lib)
(require 'org-skim-helpers)

(defcustom org-skim-bookmark-name-format "P${page}"
  "Template used to build the silent default name for new Skim bookmarks.
The following ${...} variables are expanded against the front Skim
document and the current page:

  ${title}  display name of the document (Skim `name of document')
  ${file}   basename of the document file, without extension
  ${path}   full filesystem path of the document
  ${page}   1-based page index
  ${date}   today's date as YYYY-MM-DD
  ${time}   current time as HH:MM

Unknown ${var} references are left in the output verbatim."
  :type 'string
  :group 'org-skim)

(defcustom org-skim-bookmark-prompt-format "${title} P${page}"
  "Template used to prefill the prompt when naming a new Skim bookmark.
Used when `org-skim-bookmark-add' prompts for a name (either because
`org-skim-bookmark-prompt-name' is non-nil, or a prefix argument was
given).  See `org-skim-bookmark-name-format' for the supported
${...} variables."
  :type 'string
  :group 'org-skim)

(defcustom org-skim-bookmark-prompt-name nil
  "When non-nil, always prompt for the bookmark name on add.
When nil, the formatted name from `org-skim-bookmark-name-format' is
used silently.  In both cases, calling `org-skim-bookmark-add' with a
prefix argument flips this behavior for that call."
  :type 'boolean
  :group 'org-skim)

(defcustom org-skim-bookmark-container 'auto
  "Where `org-skim-bookmark-add' places newly created bookmarks.
`auto' always prompts: a completing-read over [Global], every existing
folder bookmark, and [New folder...].  nil silently adds the bookmark at
the top level.  A string names a folder bookmark; if no folder of that
name exists, `org-skim-bookmark-add' asks before creating it."
  :type '(choice (const :tag "Prompt every time" auto)
                 (const :tag "Top level (global)" nil)
                 (string :tag "Folder name"))
  :group 'org-skim)

(defun org-skim--folder-bookmark-names ()
  "Return the slash-joined paths of every folder bookmark in Skim.
Nested folders are rendered as \"Parent / Child\"."
  (let ((raw (org-skim--run-applescript
              "on walkFolders(parentRef, folderPath, acc)
        set folderKind to \"folder bookmark\"
        tell application \"Skim\"
                set n to count of bookmarks of parentRef
                repeat with i from 1 to n
                        set b to bookmark i of parentRef
                        if (type of b as string) is folderKind then
                                set fn to name of b
                                if folderPath is \"\" then
                                        set subPath to fn
                                else
                                        set subPath to folderPath & \" / \" & fn
                                end if
                                set acc to acc & subPath & linefeed
                                set acc to my walkFolders(b, subPath, acc)
                        end if
                end repeat
        end tell
        return acc
end walkFolders

set folderKind to \"folder bookmark\"
set acc to \"\"
tell application \"Skim\"
        set n to count of bookmarks
        repeat with i from 1 to n
                set b to bookmark i
                if (type of b as string) is folderKind then
                        set fn to name of b
                        set acc to acc & fn & linefeed
                        set acc to my walkFolders(b, fn, acc)
                end if
        end repeat
end tell
return acc")))
    (split-string (or raw "") "\n" t)))

(defun org-skim--folder-exists-p (folder)
  "Return non-nil if FOLDER (slash path) names an existing folder bookmark."
  (and (stringp folder)
       (not (string-empty-p folder))
       (member folder (org-skim--folder-bookmark-names))))

(defun org-skim--folder-path-segments (folder)
  "Split FOLDER (\"A / B / C\") into a list of segment names."
  (mapcar #'string-trim (split-string folder " */ *" t)))

(defun org-skim--make-folder (folder)
  "Create the folder-bookmark path FOLDER (\"A / B / C\") in Skim.
Any missing ancestor folder along the path is created."
  (let* ((segments (org-skim--folder-path-segments folder))
         (joined (mapconcat #'org-skim--applescript-quote segments ", ")))
    (org-skim--run-applescript
     (format "on ensurePath(segments)
        tell application \"Skim\"
                set parentRef to application \"Skim\"
                repeat with seg in segments
                        set found to missing value
                        set n to count of bookmarks of parentRef
                        repeat with i from 1 to n
                                set b to bookmark i of parentRef
                                if (type of b as string) is \"folder bookmark\" and (name of b) is (seg as text) then
                                        set found to b
                                        exit repeat
                                end if
                        end repeat
                        if found is missing value then
                                set found to make new bookmark at end of bookmarks of parentRef with properties {type:folder bookmark, name:(seg as text)}
                        end if
                        set parentRef to found
                end repeat
        end tell
end ensurePath

ensurePath({%s})"
             joined))))

(defun org-skim--ensure-folder (folder)
  "Ensure FOLDER exists in Skim, prompting before creating it.
Returns FOLDER if available, signals an error if the user declines."
  (cond
   ((null folder) nil)
   ((org-skim--folder-exists-p folder) folder)
   ((y-or-n-p (format "Folder bookmark %S does not exist.  Create it? " folder))
    (org-skim--make-folder folder)
    folder)
   (t (user-error "org-skim: aborted, folder bookmark %S does not exist" folder))))

(defun org-skim--read-container ()
  "Prompt for a bookmark container.  Return nil for top-level or a folder name."
  (let* ((folders (org-skim--folder-bookmark-names))
         (global "[Global]")
         (new "[New folder...]")
         (choices (append (list global) folders (list new)))
         (pick (completing-read "Add bookmark to: " choices nil t)))
    (cond
     ((string= pick global) nil)
     ((string= pick new)
      (let ((name (read-string "New folder name: ")))
        (when (string-empty-p name)
          (user-error "org-skim: empty folder name"))
        (unless (org-skim--folder-exists-p name)
          (org-skim--make-folder name))
        name))
     (t pick))))

(defun org-skim--resolve-container ()
  "Resolve `org-skim-bookmark-container' to nil or a folder name."
  (pcase org-skim-bookmark-container
    ('auto (org-skim--read-container))
    ('nil nil)
    ((pred stringp) (org-skim--ensure-folder org-skim-bookmark-container))
    (other (error "org-skim: invalid `org-skim-bookmark-container' %S" other))))

(defun org-skim-bookmark-list ()
  "Return all of Skim's file bookmarks as `((folder name path page) ...).
FOLDER is the containing folder bookmark's name, or nil for top level.
Entries are sorted by folder name (top level first), then by the
bookmark's file basename, then by bookmark name."
  (let* ((raw (org-skim--run-applescript
               "on walk(parentRef, folderPath, acc)
        set fileKind to \"file bookmark\"
        set folderKind to \"folder bookmark\"
        tell application \"Skim\"
                set n to count of bookmarks of parentRef
                repeat with i from 1 to n
                        set b to bookmark i of parentRef
                        set bType to (type of b as string)
                        set bName to name of b
                        if bType is fileKind then
                                try
                                        set p to POSIX path of (file of b as alias)
                                        set pg to page index of b
                                        set acc to acc & folderPath & tab & bName & tab & p & tab & (pg as string) & linefeed
                                end try
                        else if bType is folderKind then
                                if folderPath is \"\" then
                                        set subPath to bName
                                else
                                        set subPath to folderPath & \" / \" & bName
                                end if
                                set acc to my walk(b, subPath, acc)
                        end if
                end repeat
        end tell
        return acc
end walk

set fileKind to \"file bookmark\"
set folderKind to \"folder bookmark\"
set acc to \"\"
tell application \"Skim\"
        set n to count of bookmarks
        repeat with i from 1 to n
                set b to bookmark i
                set bType to (type of b as string)
                set bName to name of b
                if bType is fileKind then
                        try
                                set p to POSIX path of (file of b as alias)
                                set pg to page index of b
                                set acc to acc & tab & bName & tab & p & tab & (pg as string) & linefeed
                        end try
                else if bType is folderKind then
                        set acc to my walk(b, bName, acc)
                end if
        end repeat
end tell
return acc"))
         (items (delq nil
                      (mapcar (lambda (line)
                                (let ((parts (split-string line "\t")))
                                  (when (= (length parts) 4)
                                    (let ((folder (nth 0 parts)))
                                      (list (if (string-empty-p folder) nil folder)
                                            (nth 1 parts)
                                            (nth 2 parts)
                                            (string-to-number (nth 3 parts)))))))
                              (split-string (or raw "") "\n" t))))
         (sorted (sort items
                       (lambda (a b)
                         (let ((fa (nth 0 a)) (fb (nth 0 b)))
                           (cond
                            ((and (null fa) (not (null fb))) t)
                            ((and (not (null fa)) (null fb)) nil)
                            ((and fa fb (not (string= fa fb))) (string< fa fb))
                            (t (let ((ba (file-name-nondirectory (nth 2 a)))
                                     (bb (file-name-nondirectory (nth 2 b))))
                                 (if (not (string= ba bb))
                                     (string< ba bb)
                                   (string< (nth 1 a) (nth 1 b))))))))))
         (counts (make-hash-table :test 'equal)))
    (mapcar (lambda (it)
              (let* ((key (list (nth 1 it) (nth 2 it) (nth 3 it)))
                     (n (1+ (gethash key counts 0))))
                (puthash key n counts)
                (append it (list n))))
            sorted)))

(defun org-skim--read-bookmark (prompt)
  "Read a bookmark via `completing-read'.
Returns the (folder name path page) list."
  (let* ((items (org-skim-bookmark-list))
         (_ (when (null items) (user-error "org-skim: no bookmarks in Skim")))
         (base-labels (mapcar (lambda (it)
                                (let ((folder (nth 0 it))
                                      (name (nth 1 it))
                                      (file (file-name-nondirectory (nth 2 it)))
                                      (page (nth 3 it)))
                                  (if folder
                                      (format "%s / %s (%s - p.%d)" folder name file page)
                                    (format "%s (%s - p.%d)" name file page))))
                              items))
         (counts (make-hash-table :test 'equal))
         (seen (make-hash-table :test 'equal))
         (_ (dolist (l base-labels)
              (puthash l (1+ (gethash l counts 0)) counts)))
         (labels (mapcar (lambda (l)
                           (if (> (gethash l counts) 1)
                               (let ((n (1+ (gethash l seen 0))))
                                 (puthash l n seen)
                                 (format "%s #%d" l n))
                             l))
                         base-labels))
         (table (cl-mapcar #'cons labels items))
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                         (complete-with-action action labels string pred))))
         (pick (completing-read prompt collection nil t)))
    (cdr (assoc pick table))))

;;;###autoload
(defun org-skim-bookmark-add (&optional arg)
  "Add a bookmark in Skim at the current page of the front document.
The name defaults silently to `org-skim-bookmark-name-format'.  With a
prefix ARG, or when `org-skim-bookmark-prompt-name' is non-nil, prompt
for a name prefilled with `org-skim-bookmark-prompt-format'.  ARG flips
that behavior for one call.
The container is chosen per `org-skim-bookmark-container'."
  (interactive "P")
  (let* ((info (org-skim--front-document-info))
         (prompt (if arg
                     (not org-skim-bookmark-prompt-name)
                   org-skim-bookmark-prompt-name))
         (name (if prompt
                   (read-string "Bookmark name: "
                                (org-skim--expand-template
                                 org-skim-bookmark-prompt-format info))
                 (org-skim--expand-template org-skim-bookmark-name-format info)))
         (folder (org-skim--resolve-container))
         (page (cdr (assq 'page info)))
         (path (cdr (assq 'path info)))
         (segments (and folder (org-skim--folder-path-segments folder)))
         (segments-as (if segments
                          (concat "{" (mapconcat #'org-skim--applescript-quote segments ", ") "}")
                        "{}"))
         (script
          (format "on findFolder(segments)
        tell application \"Skim\"
                set parentRef to application \"Skim\"
                repeat with seg in segments
                        set found to missing value
                        set n to count of bookmarks of parentRef
                        repeat with i from 1 to n
                                set b to bookmark i of parentRef
                                if (type of b as string) is \"folder bookmark\" and (name of b) is (seg as text) then
                                        set found to b
                                        exit repeat
                                end if
                        end repeat
                        if found is missing value then error \"Folder bookmark not found: \" & (seg as text)
                        set parentRef to found
                end repeat
                return parentRef
        end tell
end findFolder

tell application \"Skim\"
        set theFile to POSIX file %s as alias
        set theName to %s
        set thePage to %d
        set segments to %s
        if (count of segments) is 0 then
                set parentRef to application \"Skim\"
        else
                set parentRef to my findFolder(segments)
        end if
        set newBM to make new bookmark at end of bookmarks of parentRef with properties {type:file bookmark, name:theName, file:theFile}
        set page index of newBM to thePage
end tell"
                  (org-skim--applescript-quote path)
                  (org-skim--applescript-quote name)
                  page
                  segments-as)))
    (org-skim--run-applescript script)
    (message "org-skim: bookmark %S added%s"
             name (if folder (format " in %S" folder) ""))))

;;;###autoload
(defun org-skim-bookmark-remove ()
  "Remove a Skim bookmark chosen by completion."
  (interactive)
  (let* ((pick (org-skim--read-bookmark "Remove bookmark: "))
         (name (nth 1 pick))
         (path (nth 2 pick))
         (page (nth 3 pick))
         (occ (nth 4 pick)))
    (let ((result (org-skim--run-applescript
     (format "on findAndDelete(parentRef, thePath, theName, thePage, state)
        set fileKind to \"file bookmark\"
        set folderKind to \"folder bookmark\"
        tell application \"Skim\"
                set n to count of bookmarks of parentRef
                repeat with i from 1 to n
                        if item 1 of state is 0 then
                                set b to bookmark i of parentRef
                                set bType to (type of b as string)
                                if bType is fileKind then
                                        try
                                                set bp to POSIX path of (file of b as alias)
                                                if (name of b) is theName and (page index of b) is thePage and bp is thePath then
                                                        set item 2 of state to (item 2 of state) - 1
                                                        if item 2 of state is 0 then
                                                                delete b
                                                                set item 1 of state to 1
                                                        end if
                                                end if
                                        end try
                                else if bType is folderKind then
                                        set state to my findAndDelete(b, thePath, theName, thePage, state)
                                end if
                        end if
                end repeat
        end tell
        return state
end findAndDelete

set thePath to %s
set theName to %s
set thePage to %d
set targetOcc to %d
set state to {0, targetOcc}
set fileKind to \"file bookmark\"
set folderKind to \"folder bookmark\"
tell application \"Skim\"
        set n to count of bookmarks
        repeat with i from 1 to n
                if item 1 of state is 0 then
                        set b to bookmark i
                        set bType to (type of b as string)
                        if bType is fileKind then
                                try
                                        set bp to POSIX path of (file of b as alias)
                                        if (name of b) is theName and (page index of b) is thePage and bp is thePath then
                                                set item 2 of state to (item 2 of state) - 1
                                                if item 2 of state is 0 then
                                                        delete b
                                                        set item 1 of state to 1
                                                end if
                                        end if
                                end try
                        else if bType is folderKind then
                                set state to my findAndDelete(b, thePath, theName, thePage, state)
                        end if
                end if
        end repeat
end tell
return (item 1 of state) as string"
             (org-skim--applescript-quote path)
             (org-skim--applescript-quote name)
             page
             occ))))
      (if (and (stringp result) (string= (string-trim result) "0"))
          (user-error "org-skim: bookmark %S not found in Skim" name)
        (message "org-skim: bookmark %S removed" name)))))

;;;###autoload
(defun org-skim-goto-bookmark ()
  "Open a Skim bookmark chosen by completion.
Opens the bookmark's PDF in Skim if it is not already open, brings its
window to the front, and jumps to the bookmarked page."
  (interactive)
  (let* ((pick (org-skim--read-bookmark "Go to bookmark: "))
         (path (nth 2 pick))
         (page (nth 3 pick)))
    (org-skim--run-applescript
     (format "set thePath to %s
set thePage to %d
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
        tell targetDoc to go it to page thePage of it
end tell"
             (org-skim--applescript-quote path)
             page))))

(provide 'org-skim-bookmarks)

;;; org-skim-bookmarks.el ends here
