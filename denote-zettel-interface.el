;;; denote-zettel-interface.el --- Zettelkasten interfaces for Denote.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Homepage: https://github.com/krisbalintona/denote-zettel-interface
;; Version: 0.2.2
;; Package-Requires: ((emacs "29.1") (org "9.3"))
;; Keywords: tools, extensions, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Useful interfaces for those who use Denote.el as a Zettelkasten, especially
;; in a similar manner as an analog Zettelkasten.

;;; Code:
(require 'tablist)
(require 'denote)
(require 's)
(require 'subr-x)
(require 'ol)

;;;; Variables
;;;;; Customizable
(defgroup denote-zettel-interface ()
  "Interfaces for denote files."
  :group 'files)

(defcustom denote-zettel-interface-starting-filter "."
  "Default regexp filter for notes."
  :type 'sexp
  :group 'denote-zettel-interface)

(defcustom denote-zettel-interface-starting-filter-presets "=="
  "The common `denote-zettel-interface-starting-filter' filters."
  :type 'regexp
  :group 'denote-zettel-interface)

(defcustom denote-zettel-interface-signature-column-width 10
  "Width for signature column."
  :type 'natnum
  :group 'denote-zettel-interface)

(defcustom denote-zettel-interface-title-column-width 85
  "Width for title column."
  :type 'natnum
  :group 'denote-zettel-interface)

(defcustom denote-zettel-interface-keyword-column-width 30
  "Width for keywords column."
  :type 'natnum
  :group 'denote-zettel-interface)

(defcustom denote-zettel-interface-unsorted-signature "000"
  "Special signature denoting \"unsorted\" note."
  :type 'string
  :group 'denote-zettel-interface)

;;;;; Internal
(defvar denote-zettel-interface--id-to-path-cache nil
  "Signature cache for `denote-zettel-interface--get-entry-path'.")

(defvar denote-zettel-interface--signature-propertize-cache nil
  "Signature cache for `denote-zettel-interface--signature-propertize'.")

;; REVIEW 2024-09-08: Reevaluate whether this is needed or not with our new
;; implementation of `denote-zettel-interface--signature-lessp'.  I anticipate the
;; current implementation to be performant enough.
(defvar denote-zettel-interface--signature-sort-cache nil
  "Signature cache for sorting via `denote-zettel-interface--signature-lessp'.")

(defvar denote-zettel-interface--signature-relations
  '("Sibling" "Child" "Top-level")
  "List of possible note relations based on signatures.
See `denote-zettel-interface--determine-new-signature' docstring for more
information.")

;;;; Functions
;;;;; Signatures
;;;;;; Parsing
(defun denote-zettel-interface--signature-normalize (sig)
  "Normalized SIG into a signature whose parts are separated by \"=\".

This means that every transition from number to letter or letter to
number warrants the insertion of a \"=\" to delimit it.

Code supplied by Protesilaos Stavrou in
https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/."
  (replace-regexp-in-string
   "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
   (replace-regexp-in-string
    "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
    (or sig ""))))                      ; Consider when there is no signature

;; TODO 2024-09-07: Perhaps have the "standardized format" be a customizable
;; variable?  I would have to review all of the code because I started with the
;; assumption that my format would have to be adhered to, but with Protesilaos's
;; versatile way splitting of signatures, it might be possible to support
;; multiple signature formats.
(defun denote-zettel-interface--signature-unnormalize (sig)
  "Convert a normalized SIG to our standardized format.
A normalized signature's components are separated by \"=\".  However, our
standardized format only separates the first two components with a
\"=\"."
  (save-match-data
    (if (string-match "=" sig)
        (concat (substring sig 0 (match-end 0))
                (replace-regexp-in-string "=" "" sig nil nil nil (match-end 0)))
      sig)))                            ; For top-level signatures

(defun denote-zettel-interface--signature-split (sig)
  "Split SIG into Luhmann-style parts.
Returns a list of strings wherein each string is a part as described by
the docstring of `denote-zettel-interface--signature-normalize'."
  (string-split (denote-zettel-interface--signature-normalize sig) "=" t))

(defun denote-zettel-interface--signature-padded-parts (sig)
  "Add padded spaces for all parts of SIG.
For example, \"3=1b23d\" becomes \"    3=    1=    b=   23=    d\".
 This is useful for operations such as
`denote-zettel-interface--signature-lessp' which string comparators.

Modified code from Protesilaos Stavrou in
https://protesilaos.com/codelog/2024-08-01-emacs-denote-luhmann-signature-sort/."
  (combine-and-quote-strings
   (mapcar (lambda (x)
             (string-pad x 5 32 t))
           (denote-zettel-interface--signature-split sig))
   "="))

;;;;;; Calculating signatures based on relation
(defun denote-zettel-interface--first-child-signature (sig)
  "Return the first child signature of SIG.
For example, when SIG is \"13b3c,\" the returned signature is \"13b3c1.\""
  (concat sig (if (s-numeric-p (substring sig (1- (length sig)))) "a" "1")))

(defun denote-zettel-interface--next-sibling-signature (sig &optional previous)
  "Return the next sibling signature of SIG.
For example, the next sibling signature for \"a\" is \"b\", for \"9\" is
\"10\", for \"z\" is \"A\", and for \"Z\" \"aa\".

If PREVIOUS is non-nil, then the previous sibling's signature will be
calculated instead."
  (let* ((parts (denote-zettel-interface--signature-split sig))
         tail char next)
    (setq tail (car (last parts))
          char (string-to-char tail)
          next (cond ((s-numeric-p tail) ; A number
                      (number-to-string
                       (+ (if previous -1 1) (string-to-number tail))))
                     ((and (>= char 97) (< char 122)) ; Between "a" and "z"
                      (char-to-string (+ (if previous -1 1) char)))
                     ((and (>= char 65) (< char 90)) ; Between "A" and "Z"
                      (char-to-string (+ (if previous -1 1) char)))
                     ((= 122 char) "A") ; Is "z"
                     ;; REVIEW 2024-03-03: Presently, we choose to go into
                     ;; double-letters when we go above Z
                     ((= 90 char) "aa"))) ; Is "Z"
    (concat (string-remove-suffix tail sig) next)))

(defun denote-zettel-interface--parent-signature (sig)
  "Return the parent signature of SIG."
  (denote-zettel-interface--signature-unnormalize
   (string-join (butlast (denote-zettel-interface--signature-split sig)) "=")))

(defun denote-zettel-interface--determine-new-signature (sig relation &optional dir)
  "Return the next available signature relative to SIG.
The new signature depends on RELATION, a string in
`denote-zettel-interface--signature-relations'.  If RELATION is \"child\", then
return the next signature available for a new child note.  If it is
\"sibling\", then the new note will be the next available signature at
the same hierarchical level as SIG.  If it is \"top-level\", then the
next available top-level signature will be returned.  If RELATION is nil,
then it defaults to a value of \"child\".

If DIR is provided, check for the existence of signatures in that
directory rather than the entirety of variable `denote-directory'.  DIR
can also be a file.  If it is, the parent directory of that file will be
used as the directory."
  (let* ((relation (or (downcase relation) "child"))
         (dir
          (file-name-directory
           (file-relative-name (or dir default-directory) denote-directory)))
         (next-sig (pcase relation
                     ("child"
                      (denote-zettel-interface--first-child-signature sig))
                     ("sibling"
                      (denote-zettel-interface--next-sibling-signature sig))
                     ("top-level"
                      (let ((top-level-index 1))
                        (while (denote-directory-files
                                (rx (literal dir)
                                    (1+ anychar)
                                    "=="
                                    (literal (number-to-string top-level-index)) "=1"))
                          (setq top-level-index (1+ top-level-index)))
                        (concat (number-to-string top-level-index) "=1"))))))
    (while (member next-sig
                   (cl-loop for f in (denote-directory-files dir)
                            collect (denote-retrieve-filename-signature f)))
      (setq next-sig (denote-zettel-interface--next-sibling-signature next-sig)))
    next-sig))

;;;;;; Propertize
(defun denote-zettel-interface--signature-propertize (sig)
  "Return propertized SIG for hierarchical visibility."
  (or (and (not sig) "")
      (cdr (assoc-string sig denote-zettel-interface--signature-propertize-cache))
      (let* ((parts (denote-zettel-interface--signature-split sig))
             (level (1- (length parts)))
             (face
              (cond
               ((string= sig denote-zettel-interface-unsorted-signature)
                'shadow)
               ((= 0 (+ 1 (% (1- level) 8)))
                'denote-faces-signature)
               (t
                (intern (format "outline-%s" (+ 1 (% (1- level) 8)))))))
             (propertized-sig
              (replace-regexp-in-string "=" (propertize "." 'face 'shadow)
                                        (propertize sig 'face face))))
        propertized-sig)))

(defun denote-zettel-interface--file-signature-propertize (path)
  "Return propertized file signature from denote PATH identifier."
  (let ((sig (denote-retrieve-filename-signature path)))
    (denote-zettel-interface--signature-propertize sig)))

;;;;;; Caching
(defun denote-zettel-interface--generate-caches ()
  "Generate caches relevant to signatures.
Speeds up `denote-zettel-interface' expensive operations.  Populates the
following internal variables:
- `denote-zettel-interface--id-to-path-cache'
- `denote-zettel-interface--signature-propertize-cache'
- `denote-zettel-interface--signature-sort-cache'

Call this function for its side effects."
  (let* ((files (denote-directory-files))
         (sigs (cl-remove-duplicates
                (mapcar (lambda (f) (denote-retrieve-filename-signature f)) files)
                :test #'string-equal)))
    (setq denote-zettel-interface--id-to-path-cache
          (cl-loop for file in files
                   collect (cons (denote-retrieve-filename-identifier file) file))
          denote-zettel-interface--signature-sort-cache
          (sort sigs #'denote-zettel-interface--signature-lessp)
          denote-zettel-interface--signature-propertize-cache
          (cl-loop for sig in sigs
                   collect (cons sig (denote-zettel-interface--signature-propertize sig)))))
  t)

;;;;; Titles
(defun denote-zettel-interface--file-title-propertize (path)
  "Return propertized title of PATH.
If the denote file PATH has no title, return the string \"(No
Title)\".  Otherwise return PATH's title.

Determine whether a denote file has a title based on the
following rule derived from the file naming scheme:

1. If the path does not have a \"--\", it has no title."
  (let* ((titlep (string-match-p "--" path))
         (sig (denote-retrieve-filename-signature path))
         (file-type (denote-filetype-heuristics path))
         (title (denote-retrieve-front-matter-title-value path file-type))
         (face
          (cond
           ;; When SIG is the unsorted signature
           ((string= sig denote-zettel-interface-unsorted-signature)
            'denote-faces-title)
           ;; When PATH is a "indicator note" (i.e. a note strictly for
           ;; indicator subdivisions among the numbered indexes)
           ((and sig (not (cdr (denote-zettel-interface--signature-split sig))))
            'denote-faces-signature)
           ;; Otherwise
           (t 'denote-faces-title))))
    (if titlep
        (propertize title 'face face)
      (propertize "(No Title)" 'face 'shadow))))

;;;;; Keywords
(defun denote-zettel-interface--file-keywords-propertize (path)
  "Return propertized keywords of PATH."
  (string-join
   (mapcar (lambda (s) (propertize s 'face 'denote-faces-keywords))
           (denote-extract-keywords-from-path path))
   (propertize ", " 'face 'shadow)))

;;;;; Entries
(defun denote-zettel-interface--get-entry-path (&optional id)
  "Get the file path of the entry at point.
ID is the ID of the `tabulated-list' entry."
  (or (cdr (assoc-string id denote-zettel-interface--id-to-path-cache))
      (let* ((id (or id (tabulated-list-get-id)))
             (path (denote-get-path-by-id id)))
        (setf (alist-get id denote-zettel-interface--id-to-path-cache) path)
        path)))

(defun denote-zettel-interface--entries-to-paths ()
  "Return list of file paths present in the `denote-zettel-interface' buffer."
  (mapcar (lambda (entry)
            (denote-zettel-interface--get-entry-path (car entry)))
          (funcall tabulated-list-entries)))

(defun denote-zettel-interface--path-to-entry (path)
  "Convert PATH to an entry in the form of `tabulated-list-entries'."
  ;; Sometimes PATH will not exist, mostly because the file has been renamed.
  ;; The following corrects PATH.  This method relies on the persistence of the
  ;; identifier; if the identifier of the file has changed, then the function
  ;; will return nil (the file will not be shown in the denote-zettel-interface
  ;; buffer).
  (unless (file-regular-p path)
    (setq path
          (car (denote-directory-files (denote-retrieve-filename-identifier path)))))
  (when path
    `(,(denote-retrieve-filename-identifier path)
      [,(denote-zettel-interface--file-signature-propertize path)
       ,(denote-zettel-interface--file-title-propertize path)
       ,(denote-zettel-interface--file-keywords-propertize path)])))

;;;;; Sorters
(defun denote-zettel-interface--signature-lessp (sig1 sig2)
  "Compare two strings based on my signature sorting rules.
Returns t if SIG1 should be sorted before SIG2, nil otherwise.  Uses
`string<' to compare strings.

Special exception is given to the `denote-zettel-interface-unsorted-signature'
signature.  If SIG1 or SIG2 match `denote-zettel-interface-unsorted-signature',
it will be sorted such that `denote-zettel-interface-unsorted-signature' is
always less, which allows those notes to precede all other notes."
  (let ((cache denote-zettel-interface--signature-sort-cache))
    (cond
     ((string= denote-zettel-interface-unsorted-signature sig1) t)
     ((string= denote-zettel-interface-unsorted-signature sig2) nil)
     ((and (member sig1 cache) (member sig2 cache))
      (< (cl-position sig1 cache :test #'equal)
         (cl-position sig2 cache :test #'equal)))
     (t (string< (denote-zettel-interface--signature-padded-parts sig1)
                 (denote-zettel-interface--signature-padded-parts sig2))))))

(defun denote-zettel-interface--signature-sorter (a b)
  "Tabulated-list sorter for entries A and B.
Note that this function needs to be performant, otherwise
`denote-zettel-interface' loading time suffers greatly."
  (let* ((sig1 (aref (cadr a) 0))
         (sig2 (aref (cadr b) 0)))
    ;; FIXME 2024-03-04: I have to replace "."s with "=" because in
    ;; `denote-zettel-interface--path-to-entry' I do the reverse.  This is quite
    ;; fragile, so try to find a more robust alternative
    (setq sig1 (replace-regexp-in-string "\\." "=" sig1)
          sig2 (replace-regexp-in-string "\\." "=" sig2))
    (denote-zettel-interface--signature-lessp sig1 sig2)))

;;;; Major-modes and keymaps
(defvar-keymap denote-zettel-interface-mode-map
  :doc "Mode map for `denote-zettel-interface-mode'."
  :parent tablist-mode-map
  "/ d" #'denote-zettel-interface-edit-filter
  "/ D" #'denote-zettel-interface-edit-filter-presets
  "RET" #'denote-zettel-interface-goto-note
  "o" #'denote-zettel-interface-goto-note-other-window
  "C-o" #'denote-zettel-interface-display-note
  "r" #'denote-zettel-interface-set-signature-list
  "R" #'denote-zettel-interface-set-signature-minibuffer
  "w" #'denote-zettel-interface-store-link
  "M-N" #'denote-zettel-interface-filter-forward
  "M-P" #'denote-zettel-interface-filter-backward
  "M-n" #'denote-zettel-interface-filter-top-level-next
  "M-p" #'denote-zettel-interface-filter-top-level-previous
  "M-u" #'denote-zettel-interface-filter-up
  "M-d" #'denote-zettel-interface-filter-down)

(define-derived-mode denote-zettel-interface-mode tablist-mode "Denote Interface"
  "Major mode for interfacing with Denote files."
  :interactive nil
  :group 'denote-zettel-interface
  :after-hook (set (make-local-variable 'mode-line-misc-info)
                   (append
                    (list
                     (list 'denote-zettel-interface-starting-filter
                           '(:eval (format " [%s]" denote-zettel-interface-starting-filter))))))
  (unless (and denote-zettel-interface--id-to-path-cache
               denote-zettel-interface--signature-sort-cache
               denote-zettel-interface--signature-propertize-cache)
    (denote-zettel-interface--generate-caches))
  (setq tabulated-list-format
        `[("Signature" ,denote-zettel-interface-signature-column-width denote-zettel-interface--signature-sorter)
          ("Title" ,denote-zettel-interface-title-column-width t)
          ("Keywords" ,denote-zettel-interface-title-column-width nil)]
        tabulated-list-entries
        (lambda () (mapcar #'denote-zettel-interface--path-to-entry
                           (denote-directory-files denote-zettel-interface-starting-filter)))
        tabulated-list-sort-key '("Signature" . nil))
  (use-local-map denote-zettel-interface-mode-map)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun denote-zettel-interface-list (&optional name)
  "Display list of Denote files in variable `denote-directory'.
If NAME is supplied, that will be the name of the buffer."
  (interactive nil)
  (let ((buf-name (cond
                   ((stringp name) name)
                   (current-prefix-arg
                    (format "*Denote notes: %s*" (read-from-minibuffer "Name for new buffer: ")))
                   (t
                    (format "*Denote notes in %s*"
                            (abbreviate-file-name (buffer-local-value 'denote-directory (current-buffer))))))))
    ;; TODO 2024-03-28: Haven't figured out how to adhere to the current
    ;; denote-silo convention of setting `denote-directory' in .dir-locals
    (unless (get-buffer buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (setq buffer-file-coding-system 'utf-8)
        (denote-zettel-interface-mode)))
    (display-buffer buf-name)))

;;;; Commands
;;;;; Filtering
;;;###autoload
(defun denote-zettel-interface-edit-filter ()
  "Edit the currently existing filter."
  (interactive)
  (setq denote-zettel-interface-starting-filter
        (read-from-minibuffer "Filter regex: " denote-zettel-interface-starting-filter))
  (revert-buffer))

;;;###autoload
(defun denote-zettel-interface-edit-filter-presets ()
  "Edit the currently existing filter."
  (interactive)
  (setq denote-zettel-interface-starting-filter
        (completing-read "Filter preset: "
                         (remove denote-zettel-interface-starting-filter denote-zettel-interface-starting-filter-presets)))
  (revert-buffer))

;;;;; Viewing
;;;###autoload
(defun denote-zettel-interface-goto-note ()
  "Jump to the note corresponding to the entry at point."
  (interactive)
  (find-file (denote-zettel-interface--get-entry-path)))

;;;###autoload
(defun denote-zettel-interface-goto-note-other-window ()
  "Open in another window the note corresponding to the entry at point."
  (interactive)
  (find-file-other-window (denote-zettel-interface--get-entry-path)))

;;;###autoload
(defun denote-zettel-interface-display-note ()
  "Just display the current note in another window."
  (interactive)
  (display-buffer (find-file-noselect (denote-zettel-interface--get-entry-path)) t))

;;;;; Signatures
;;;###autoload
(defun denote-zettel-interface-set-signature (path new-sig)
  "Set the note at point's (in `denote-zettel-interface' buffer) signature.
Can be called interactively from a denote note or a `denote-zettel-interface'
entry.  If called non-interactively, set the signature of PATH to
NEW-SIG."
  (interactive (list (cond ((derived-mode-p 'denote-zettel-interface-mode)
                            (denote-zettel-interface--get-entry-path))
                           ((denote-file-is-note-p (buffer-file-name))
                            (buffer-file-name))
                           (t
                            (user-error "Must use in `denote-zettel-interface' or a Denote note!")))
                     nil))
  (let* ((path (or path (denote-zettel-interface--get-entry-path)))
         (file-type (denote-filetype-heuristics path))
         (title (denote-retrieve-title-value path file-type))
         (initial-sig (denote-retrieve-filename-signature path))
         (new-sig (or new-sig
                      (denote-signature-prompt
                       (unless (string= initial-sig denote-zettel-interface-unsorted-signature)
                         initial-sig)
                       "Choose new signature")))
         (keywords
          (denote-retrieve-front-matter-keywords-value path file-type))
         (date (denote-retrieve-filename-identifier path)))
    (denote-rename-file path title keywords new-sig date)))

(defun denote-zettel-interface--group-text-property (text sig)
  "Set the `denote-zettel-interface-sig' text property in TEXT.
The value the `denote-zettel-interface-sig' property will be set to will be the
leading component of SIG.  For example, the leading component for \"3=b\"
is \"3\".  This function only produces side effects."
  (add-text-properties 0
                       (length text)
                       (list 'denote-zettel-interface-sig (if sig
                                                              (car (denote-zettel-interface--signature-split sig))
                                                            "No signature"))
                       text))

;;;###autoload
(defun denote-zettel-interface-set-signature-minibuffer (files)
  "Set the note at point's signature by selecting another note.
Select another note and choose whether to be its the sibling or child.

Also accepts FILES, which are the list of file paths which are
considered.

If nil or called interactively, then defaults `denote-directory-files'
constrained to notes with signatures (i.e. \"==\") and are in the
current subdirectory (this is my bespoke desired behavior), as well as
the :omit-current non-nil.  Otherwise,when called interactively in
`denote-zettel-interface', it will be the value of
`denote-zettel-interface--entries-to-paths'."
  (interactive (list (if (eq major-mode 'denote-zettel-interface-mode)
                         (denote-zettel-interface--entries-to-paths)
                       (denote-directory-files
                        (rx (literal (car (last (butlast (file-name-split (buffer-file-name)) 1))))
                            "/" (* alnum) "==")
                        :omit-current))))
  (let* ((file-at-point (cond ((derived-mode-p 'denote-zettel-interface-mode)
                               (denote-zettel-interface--get-entry-path))
                              ((denote-file-is-note-p (buffer-file-name))
                               (buffer-file-name))
                              (t
                               (user-error "Must use in `denote-zettel-interface' or a Denote note!"))))
         (files (remove file-at-point
                        (or files (denote-directory-files
                                   (rx (literal (car (last (butlast (file-name-split (buffer-file-name)) 1))))
                                       "/" (* alnum) "==")
                                   :omit-current))))
         (files
          (cl-loop for file in files collect
                   (let ((sig (denote-retrieve-filename-signature file)))
                     (denote-zettel-interface--group-text-property file sig)
                     file)))
         (largest-sig-length
          (cl-loop for file in files
                   maximize (length (denote-retrieve-filename-signature file))))
         (display-sort-function
          (lambda (completions)
            (cl-sort completions
                     'denote-zettel-interface--signature-lessp
                     :key (lambda (c) (denote-retrieve-filename-signature c)))))
         (group-function
          (lambda (title transform)
            (if transform
                title
              (get-text-property 0 'denote-zettel-interface-sig title))))
         (affixation-function
          (lambda (files)
            (cl-loop for file in files collect
                     (let* ((propertized-title (denote-zettel-interface--file-title-propertize file))
                            (sig (denote-retrieve-filename-signature file))
                            (propertized-sig (denote-zettel-interface--file-signature-propertize file)))
                       (denote-zettel-interface--group-text-property propertized-title sig)
                       (list propertized-title
                             (string-pad propertized-sig (+ largest-sig-length 3))
                             nil)))))
         (selection
          (completing-read "Choose a note: "
                           (lambda (str pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   (display-sort-function . ,display-sort-function)
                                   (group-function . ,group-function)
                                   (affixation-function . ,affixation-function))
                               (complete-with-action action files str pred)))
                           nil t))
         (note-relation
          (downcase (completing-read "Choose relation: " denote-zettel-interface--signature-relations)))
         (new-sig (denote-zettel-interface--determine-new-signature
                   (denote-retrieve-filename-signature selection)
                   note-relation
                   selection)))
    (denote-zettel-interface-set-signature file-at-point new-sig)))

;;;###autoload
(defun denote-zettel-interface-set-signature-list (files)
  "Set the note at point's signature by selecting another note.
Like `denote-zettel-interface-set-signature-minibuffer' but uses
`denote-zettel-interface-selection-mode' instead of minibuffer.

FILES are the list of files shown, defaulting to all denote files.  When
\"RET\" or \"q\" is called on a note, the new signature of the file to
be modified will be set relative to that note.  See
`denote-zettel-interface--determine-new-signature' for more details."
  (interactive (list (if (eq major-mode 'denote-zettel-interface-mode)
                         (progn
                           (revert-buffer) ; Ensure entries align with changes to files
                           (cl-remove
                            ;; Ensure the note whose signature is being modified
                            ;; is not in the resultant list of files
                            (denote-zettel-interface--get-entry-path)
                            (denote-zettel-interface--entries-to-paths)
                            :test #'string-equal))
                       (denote-directory-files
                        (rx (literal (concat (car (last (butlast (file-name-split (buffer-file-name)) 1)))
                                             "/"))
                            (* alnum) "==")
                        :omit-current))))
  (let* ((file-at-point (cond ((derived-mode-p 'denote-zettel-interface-mode)
                               (denote-zettel-interface--get-entry-path))
                              ((denote-file-is-note-p (buffer-file-name))
                               (buffer-file-name))
                              (t
                               (user-error "Must use in `denote-zettel-interface' or a Denote note!"))))
         (files (remove file-at-point
                        (or files (denote-directory-files
                                   (rx (literal (car (last (butlast (file-name-split (buffer-file-name)) 1))))
                                       "/" (* alnum) "==")
                                   :omit-current))))
         (file-type (denote-filetype-heuristics file-at-point))
         (buf-name (format "*Modify the signature of %s %s*"
                           (denote-retrieve-filename-signature file-at-point)
                           (denote-retrieve-front-matter-title-value file-at-point file-type)))
         (select-func
          `(lambda ()
             "Change desired note's signature relate to the chosen note."
             (interactive)
             (let* ((selection (denote-zettel-interface--get-entry-path))
                    (note-relation
                     (downcase (completing-read "Choose relation: "
                                                denote-zettel-interface--signature-relations)))
                    (new-sig (denote-zettel-interface--determine-new-signature
                              (denote-retrieve-filename-signature selection)
                              note-relation
                              selection)))
               (denote-zettel-interface-set-signature ,file-at-point new-sig)
               (kill-buffer ,buf-name))))
         (confirm-quit-func
          `(lambda ()
             "Kill buffer with confirmation"
             (interactive)
             (when (y-or-n-p (format "Leave before modifying signature of %s?"
                                     ,(denote-retrieve-front-matter-title-value file-at-point file-type)))
               (kill-buffer ,buf-name))))
         (keymap (make-sparse-keymap)))
    (denote-zettel-interface-list buf-name)
    (with-current-buffer buf-name
      (setq tabulated-list-entries
            (lambda () (mapcar #'denote-zettel-interface--path-to-entry files)))
      (revert-buffer)
      (set-keymap-parent keymap denote-zettel-interface-mode-map)
      (define-key keymap (kbd "RET") select-func)
      (define-key keymap (kbd "q") confirm-quit-func)
      (use-local-map keymap))))

;;;;; Navigation
(defun denote-zettel-interface--validate-signature-cycling (sig &optional backward)
  "Ensure that SIG cycles if such a file does not exist.
If a file with SIG does not exist (given the value of
`denote-zettel-interface-starting-filter'), then this function will return the
next signature of an existing file according to BACKWARD.  That is, in
the cases that SIG does not have an existing note, if BACKWARD is nil
then the lowest signature will be returned, but if BACKWARD is non-nil
then the highest signature will be returned."
  (if (denote-directory-files
       (rx (regexp denote-zettel-interface-starting-filter) "==" (literal sig)))
      sig
    (let* ((parts (denote-zettel-interface--signature-split sig))
           (all-but-last-part (butlast (denote-zettel-interface--signature-split sig)))
           (last-part (car (last parts)))
           (last-part-num-p (string-match-p "^[0-9]+$" last-part)))
      (if backward
          (car
           (last
            (cl-remove-if-not
             (lambda (s)
               (= (length parts) (length (denote-zettel-interface--signature-split s))))
             (cl-sort
              (mapcar
               (lambda (f) (denote-retrieve-filename-signature f))
               (denote-directory-files
                (rx (regexp denote-zettel-interface-starting-filter)
                    "=="
                    (literal (denote-zettel-interface--parent-signature sig)))))
              'denote-zettel-interface--signature-lessp))))
        (denote-zettel-interface--signature-unnormalize
         (string-join
          (append all-but-last-part
                  (list (if last-part-num-p "1" "a")))
          "="))))))

;;;###autoload
(defun denote-zettel-interface-filter-top-level-next (N)
  "Filter the buffer to the next index top-level notes.
Go forward N top-levels.

Uses `tablist' filters."
  (interactive (list (or (and (numberp current-prefix-arg)
                              current-prefix-arg)
                         1)))
  (let* ((files (denote-directory-files denote-zettel-interface-starting-filter))
         (signature-heads (mapcar (lambda (f)
                                    (car
                                     (denote-zettel-interface--signature-split
                                      (denote-retrieve-filename-signature f))))
                                  files))
         (min-top-level
          (cl-loop for s in signature-heads minimize (string-to-number (or s ""))))
         (max-top-level
          (cl-loop for s in signature-heads maximize (string-to-number (or s ""))))
         (path (denote-zettel-interface--get-entry-path))
         (sig (denote-retrieve-filename-signature path))
         (sig-top-level
          (string-to-number (car (denote-zettel-interface--signature-split sig))))
         (next-top-level (+ N sig-top-level))
         (next-top-level
          (cond
           ((and (<= min-top-level next-top-level) (<= next-top-level max-top-level))
            next-top-level)
           ((< next-top-level min-top-level)
            (+ max-top-level sig-top-level))
           ((< max-top-level next-top-level)
            (- sig-top-level max-top-level))))
         (regexp
          (if (= next-top-level (string-to-number denote-zettel-interface-unsorted-signature))
              (rx bol (literal denote-zettel-interface-unsorted-signature))
            (rx bol (literal (number-to-string next-top-level)) (or eol (not digit))))))
    ;; OPTIMIZE 2024-03-17: Right now this assumes that the head of the filters
    ;; is a previous filter made by this command.
    (tablist-pop-filter 1)
    (tablist-push-regexp-filter "Signature" regexp)))

;;;###autoload
(defun denote-zettel-interface-filter-top-level-previous (N)
  "Filter the buffer to the next index top-level notes.
Go backward N top-levels.

Uses `tablist' filters."
  (interactive (list (or (and (numberp current-prefix-arg)
                              (- current-prefix-arg))
                         -1)))
  (denote-zettel-interface-filter-top-level-next N))

;;;###autoload
(defun denote-zettel-interface-filter-forward (N)
  "Filter the buffer to the next set of notes of the same level.
Go forward N levels, defaulting to 1.

Uses `tablist' filters."
  (interactive (list (or (and (numberp current-prefix-arg)
                              current-prefix-arg)
                         1)))
  (let* ((path (denote-zettel-interface--get-entry-path))
         (sig (denote-retrieve-filename-signature path))
         (sibling-sig sig)
         regexp)
    (dotimes (_ (abs N))
      (setq sibling-sig
            (denote-zettel-interface--validate-signature-cycling
             (denote-zettel-interface--next-sibling-signature sibling-sig (< N 0))
             (< N 0))))
    ;; If there are only numbers in the final sibling-sig, then that means it is
    ;; an indexical (e.g. "1" or "15" as opposed to "1=1", etc.).  In those
    ;; cases, to avoid too broad a regexp (e.g. "^2"), we need to apply a
    ;; special regexp.
    (setq regexp
          (if (string-match-p "\\`[0-9]+\\'" sibling-sig)
              (rx bol (literal sibling-sig) (or eol (not digit)))
            ;; FIXME 2024-09-07: I have to replace "."s with "=" because in
            ;; `denote-zettel-interface--path-to-entry' I do the reverse.  This is quite
            ;; fragile, so try to find a more robust alternative
            (rx bol (literal (replace-regexp-in-string "=" "." sibling-sig)))))
    ;; OPTIMIZE 2024-03-17: Right now this assumes that the head of the filters
    ;; is a previous filter made by this command.
    (tablist-pop-filter 1)
    (tablist-push-regexp-filter "Signature" regexp)))

;;;###autoload
(defun denote-zettel-interface-filter-backward (N)
  "Filter the buffer to the next set of notes of the same level.
Go backward N levels, defaulting to 1.

Uses `tablist' filters."
  (interactive (list (or (and (numberp current-prefix-arg)
                              (- current-prefix-arg))
                         -1)))
  (denote-zettel-interface-filter-forward N))

;;;###autoload
(defun denote-zettel-interface-filter-down ()
  "Filter the buffer to the children of the current note.
Uses `tablist' filters."
  (interactive)
  (let* ((path (denote-zettel-interface--get-entry-path))
         (sig (denote-retrieve-filename-signature path))
         (child-sig
          (if (string-match "=" sig)
              (denote-zettel-interface--first-child-signature sig)
            (concat sig "=1"))) ; Don't use "." yet since it breaks the following regexp search
         (regexp
          (if (denote-directory-files
               (rx (regexp denote-zettel-interface-starting-filter) "==" (literal child-sig)))
              ;; FIXME 2024-09-07: I have to replace "."s with "=" because in
              ;; `denote-zettel-interface--path-to-entry' I do the reverse.  This is
              ;; quite fragile, so try to find a more robust alternative
              (rx bol (literal (replace-regexp-in-string "=" "." child-sig)))
            (rx bol (literal (replace-regexp-in-string "=" "." sig))))))
    ;; OPTIMIZE 2024-03-17: Right now this assumes that the head of the filters
    ;; is a previous filter made by this command.
    (tablist-pop-filter 1)
    (tablist-push-regexp-filter "Signature" regexp)))

;;;###autoload
(defun denote-zettel-interface-filter-up ()
  "Filter the buffer to the parent of the current note and its children.
Uses `tablist' filters."
  (interactive)
  (let* ((path (denote-zettel-interface--get-entry-path))
         (sig (denote-retrieve-filename-signature path))
         (parent-sig (denote-zettel-interface--parent-signature sig))
         (regexp
          (cond
           ((string-empty-p parent-sig) nil)
           ((not (string-match-p "=" parent-sig))
            (rx bol (literal parent-sig) (or eol (not digit))))
           ((not (string-empty-p parent-sig))
            ;; FIXME 2024-09-07: I have to replace "."s with "=" because in
            ;; `denote-zettel-interface--path-to-entry' I do the reverse.  This is quite
            ;; fragile, so try to find a more robust alternative
            (rx bol (literal (replace-regexp-in-string "=" "."  parent-sig)))))))
    ;; OPTIMIZE 2024-03-17: Right now this assumes that the head of the filters
    ;; is a previous filter made by this command.
    (tablist-pop-filter 1)
    (when regexp
      (tablist-push-regexp-filter "Signature" regexp)
      (goto-char (point-min))
      (tablist-skip-invisible-entries)
      (goto-char (1+ (point))))))

;;;;; Storing
;;;###autoload
(defun denote-zettel-interface-store-link ()
  "Call `org-store-link' on the entry at point if an org file."
  (interactive)
  (when-let* ((file (denote-zettel-interface--get-entry-path))
              (file-id (denote-retrieve-filename-identifier file))
              (description (denote-get-link-description file))
              (orgp (string= "org" (file-name-extension file))))
    ;; Populate `org-store-link-plist'.  Inspired by `denote-link-ol-store'
    (org-link-store-props
     :type "denote"
     :description description
     :link (concat "denote:" file-id))
    ;; Then add to `org-stored-links'
    (push (list (plist-get org-store-link-plist :link)
                (plist-get org-store-link-plist :description))
          org-stored-links)
    (message "Stored %s!" (file-relative-name file denote-directory))))

;;; [End]
(provide 'denote-zettel-interface)
;;; denote-zettel-interface.el ends here
