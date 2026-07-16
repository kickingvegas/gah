;;; fj.el --- GitHub issues browser                -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient "0.9.0") (ox-gfm "1.0"))

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

;;; Code:
(require 'seq)
(require 'map)
(require 'vtable)
(require 'transient)
(require 'org-element)
(require 'esh-mode)
(require 'view)


;;; Variables

(defgroup fj nil
  "Group settings for fj.

fj is a browser for GitHub issues."
  :group 'convenience)

(defcustom fj-username nil
  "GitHub username."
  :type '(choice (const :tag "None" nil)
                 (string :tag "String Value"))
  :group 'fj)

(defcustom fj-browser-fields '("number"
                               "title"
                               "body"
                               "author"
                               "assignees"
                               "url"
                               "state"
                               "labels"
                               "createdAt"
                               "updatedAt"
                               "milestone")
  "GitHub fields to request.

The following fields must be included in this list:

- number
- title
- body
- author
- assignees
- url
- state
- labels
- createdAt
- updatedAt
- milestone

Supported fields can be found in the man page `gh-issue-view'."
  :type '(repeat string)
  :group 'fj)

(defcustom fj-request-issue-count 50
  "Count limit for requested issues."
  :type 'integer
  :group 'fj)

(defvar fj--last-repo-history nil
  "Private variable to store last used GitHub repository name.")

(defvar fj-repo-name nil
  "Local repository name.")

(defvar fj--repo-list nil
  "List of repos owned by `fj-username'.")



;;; Functions

(defun fj-read-repo (prompt)
  "Prompt the user with PROMPT, using the last history entry as the default input."
  (let* ((history 'fj--last-repo-history)  ; Define the history variable
         (last-history-entry (car (symbol-value history))) ; Get the last entry
         (repo-list (if fj--repo-list
                        fj--repo-list
                      (setq fj--repo-list (fj-list-repos)))))
    (string-trim
     (completing-read prompt
                      repo-list
                      nil
                      nil
                      last-history-entry
                      history))))


(defun fj-md2org (buf)
  "Convert BUF text format from Markdown to Org."
  (save-excursion
    (with-temp-buffer
      (insert buf)
      (shell-command-on-region (point-min) (point-max)
                               "pandoc --to=org --wrap=preserve"
                               (current-buffer)
                               t)
      (buffer-string))))

(defun fj-format-labels (labels)
  "Convert LABELS to a comma-separated string.

LABELS is a vector of hash-tables, each hash-table corresponding
to the JSON dictionary containing label information returned by
gh."
  (let ((temp-list (mapcar (lambda (label)
                             (map-elt label "name"))
                           labels)))

    (string-join temp-list ", ")))

(defun fj-iso8601-to-local-org-time (timestamp)
  "Convert an ISO 8601 UTC TIMESTAMP to local Org timestamp."
  (let* ((time-components (parse-time-string timestamp))
         (utc-time (encode-time time-components))
         (local-time (current-time-zone utc-time)))
    (format-time-string "%Y-%m-%d %H:%M:%S" (apply 'encode-time time-components) local-time)))

;; ;; Example usage
;; (let ((utc-timestamp "2024-12-23T02:42:41Z"))
;;   (message "Local time: %s" (fj-iso8601-to-local-org-time utc-timestamp)))

;; (defvar-keymap vtable-map
;;   "S" #'vtable-sort-by-current-column
;;   "{" #'vtable-narrow-current-column
;;   "}" #'vtable-widen-current-column
;;   "g" #'vtable-revert-command
;;   "M-<left>" #'vtable-previous-column
;;   "M-<right>" #'vtable-next-column)

(keymap-set vtable-map "TAB" #'vtable-next-column)
(keymap-set vtable-map "<backtab>" #'vtable-previous-column)

(defun fj-browse-url (&optional issue)
  "Open URL in ISSUE."
  (interactive)
  (let* ((issue (if (not issue)
                    (vtable-current-object)
                  issue))
         (url (map-elt issue "url")))
    (browse-url url)))

(defun fj-format-buffer-name (issue)
  "Generate buffer name from ISSUE."

  (let ((repo fj-repo-name)
        (number (map-elt issue "number"))
        (title (map-elt issue "title")))
    (format "*%s: #%d %s*"
            repo
            number
            title)))

(defun fj-copy-issue (&optional issue)
  "Copy ISSUE to `kill-ring'."
  (interactive)
  (let* ((issue (if (not issue)
                    (vtable-current-object)
                  issue))

         (bufname (fj-format-buffer-name issue)))
    (kill-new (fj-render-issue-as-org
               issue
               (string-trim (car fj--last-repo-history))))
    (message "Copied %s to kill ring" bufname)))


(defun fj-switch-to-issue ()
  "Switch to issue."
  (interactive)
  (let* ((issue (vtable-current-object))
         (bufname (fj-format-buffer-name issue)))

    (if (get-buffer bufname)
        (select-window (get-buffer-window (switch-to-buffer-other-window bufname)))

      (fj-browse-issue issue)
      (select-window (get-buffer-window (switch-to-buffer-other-window bufname))))))


(defun fj-render-issue-as-org (issue repo)
  "Render ISSUE in REPO in Org format."

  (let* ((number (map-elt issue "number"))
         (title (map-elt issue "title"))
         (author (map-elt issue "author"))
         (assignees (map-elt issue "assignees"))
         (body (map-elt issue "body"))
         (url (map-elt issue "url"))
         (state (map-elt issue "state"))
         (labels (map-elt issue "labels"))
         (createdAt (map-elt issue "createdAt"))
         (updatedAt (map-elt issue "updatedAt"))
         (milestone (map-elt issue "milestone"))
         (created (fj-iso8601-to-local-org-time createdAt))
         (updated (fj-iso8601-to-local-org-time updatedAt))
         (temp-list ()))

    (push (format "** TODO %s #%d: %s" repo number title) temp-list)
    (push ":PROPERTIES:" temp-list)
    (push (format ":CREATED: %s" created) temp-list)
    (push (format ":UPDATED: %s" updated) temp-list)
    (if milestone
        (push (format ":MILESTONE: %s" (map-elt milestone "title")) temp-list))
    (if labels
        (push (format ":LABELS: %s" (fj-format-labels (map-elt issue "labels"))) temp-list))

    (if assignees
        (push (format ":ASSIGNEES: %s"
                      (string-join
                       (mapcar (lambda (x) (map-elt x "name")) assignees) ", "))
              temp-list))

    (push (format ":STATE: %s" state) temp-list)
    (push (format ":AUTHOR: %s"
                  (map-elt author "name"))
          temp-list)

    (push ":END:" temp-list)
    (push "" temp-list)
    (push (format "[[%s][%s #%d: %s]]" url repo number title) temp-list)
    (push "" temp-list)
    (push (fj-md2org body) temp-list)
    (push "" temp-list)
    (string-join (seq-reverse temp-list) "\n")))

(defun fj-browse-issue (&optional issue)
  "Browse ISSUE."
  (interactive)
  (let* ((issue (if (not issue)
                    (vtable-current-object)
                  issue))
         (issue-window (selected-window))
         (repo fj-repo-name)
         (body (fj-render-issue-as-org issue repo))
         (bufname (fj-format-buffer-name issue))
         (buf (get-buffer-create bufname)))

    (switch-to-buffer-other-window buf)

    (when (= (buffer-size) 0)
      (org-mode)
      (setq-local fj-repo-name repo)
      (insert body)
      (goto-char (point-min))
      (read-only-mode))

    (select-window issue-window)))

(defface fj-issues-face
  '((t (:inherit variable-pitch :extend t :height 0.9)))
  "Issues face.")

(defun fj-next-line ()
  "Next line."
  (interactive)
  (forward-line 1)
  (fj-browse-issue (vtable-current-object)))

(defun fj-previous-line ()
  "Previous line."
  (interactive)
  (forward-line -1)
  (fj-browse-issue (vtable-current-object)))

(defun fj-request-issues (repo)
  "Request issues for REPO."
  (let* ((fields fj-browser-fields)
         (cmd-list (list "gh"
                         "--repo"
                         (format "'%s'" repo)
                         "issue"
                         "list"
                         "--limit"
                         (number-to-string fj-request-issue-count)
                         "--json"
                         (string-join fields ","))))

    (json-parse-string (shell-command-to-string
                        (string-join cmd-list " "))
                       :null-object nil)))

(defun fj-refresh-issues ()
  "Refresh issues."
  (let* ((repo fj-repo-name)
         (issues (fj-request-issues repo))
         (count (length issues)))
    ;; !!! vtable has a bug debbugs #69454 where an empty table is not handled
    ;; !!! correctly due to a bug in column width handling.
    (if (= count 0)
        nil
      (message "Refreshed %s issues (%d)" repo count)
      (seq-into issues 'list))))

(defun fj-kill-all-repo-buffers ()
  "Kill current repo buffers."
  (interactive)
  (let* ((repo fj-repo-name)
         (pat (format "*%s" repo))
         (blist (buffer-list))
         (repo-buffers (seq-filter
                        (lambda (b)
                          (let ((bufname (buffer-name b)))
                            (string-match pat bufname)))
                        blist)))
    (mapc (lambda (b)
            (kill-buffer b))
          repo-buffers)
    (message "Killed all %s buffers" repo)))


(defun fj-issues ()
  "Put current issues for a GitHub repository in a vtable.

The command prompts the user for a GitHub repository, which if it
exists will then retrieve the current list of issues for it via gh."
  (interactive)

  (if (and fj-username
           (stringp fj-username)
           (not (string-equal fj-username "")))

      (let* ((repo (fj-read-repo "Repo: "))
             (repo-buffer-name (format "*fj: %s*" repo)))

        (get-buffer-create repo-buffer-name)
        (switch-to-buffer (set-buffer repo-buffer-name))
        (toggle-truncate-lines t)
        (setq-local fj-repo-name repo)

        (read-only-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (make-vtable
           :columns '((:name "#")
                      (:name "Title" :width 40)
                      (:name "Author")
                      (:name "Assignees")
                      (:name "Labels")
                      (:name "Milestone" :primary ascend)
                      ;; (:name "Updated" :displayer (lambda (value max-width table)
                      ;;                               (propertize value 'face 'fixed-pitch)))
                      (:name "Updated")
                      (:name "Created"))

           :face 'fj-issues-face

           :actions '("c" fj-copy-issue
                      "<double-mouse-1>" fj-browse-url)

           :objects-function #'fj-refresh-issues

           :getter (lambda (issue column table)
                     (pcase (vtable-column table column)
                       ("#" (map-elt issue "number"))
                       ("Title" (map-elt issue "title"))
                       ("Author" (map-elt (map-elt issue "author") "name"))
                       ("Assignees" (string-join
                                     (mapcar (lambda (x) (map-elt x "name"))
                                             (map-elt issue "assignees"))
                                     ", "))
                       ("Labels" (fj-format-labels (map-elt issue "labels")))
                       ("Milestone" (let ((milestone (map-elt issue "milestone")))
                                      (if milestone
                                          (map-elt milestone "title")
                                        "")))
                       ("Created" (fj-iso8601-to-local-org-time (map-elt issue "createdAt")))
                       ("Updated" (fj-iso8601-to-local-org-time (map-elt issue "updatedAt")))))
           :keymap (define-keymap
                     "RET" #'fj-switch-to-issue
                     "q" #'quit-window
                     "b" #'fj-browse-url
                     "Q" #'View-kill-and-leave
                     "n" #'fj-next-line
                     "p" #'fj-previous-line
                     "j" #'fj-next-line
                     "t" #'toggle-truncate-lines
                     "k" #'fj-previous-line
                     "C-o" #'fj-issues-tmenu
                     "N" #'fj-request-issue-create
                     "K" #'fj-kill-all-repo-buffers))))
    (error "The variable ‘fj-username’ must be set to the GitHub user name")))

(defalias 'fj #'fj-issues
  "Alias for `fj-issues'.")

(defun fj-request-list-repos ()
  "List repos owned by user."

  (let ((cmd-list '("gh"
                     "repo"
                     "list"
                     "-L"
                     "1000"
                     "--json"
                     "name,url")))
    (json-parse-string
     (shell-command-to-string (string-join cmd-list " "))
     :null-object nil)))

(defun fj-list-repos ()
  "List repos."
  (let* ((response (fj-request-list-repos))
         (names (seq-map
                 (lambda (e)
                   (file-name-concat fj-username (map-elt e "name")))
                 response)))
    names))

(defun fj-request-issue-create (&optional repo)
  "Request issue create with REPO."
  (interactive)

  ;; Can't use (with-editor-shell-command cmd) because gh tries to
  ;; detect if running in a TTY

  (let* ((repo (if repo
                   repo
                 (fj-read-repo "Repo: ")))

         (cmdlist (list "gh"
                        "issue"
                        "create"
                        "--repo"
                        repo
                        "--editor"))
         (cmd (string-join cmdlist " ")))

    (unless (get-buffer "*eshell*")
      (eshell))

    (let ((esb (get-buffer "*eshell*")))
      (when esb
        (with-current-buffer esb
          (goto-char (point-max))
          (insert cmd)
          (eshell-send-input))))))

(defun fj-create-issue ()
  "Create GH issue."
  (interactive)

  (if (and (derived-mode-p 'org-mode) fj-username)
      (save-excursion
        (outline-back-to-heading)
        (let* ((repo (fj-read-repo "Repo: "))
               (element (org-element-at-point))
               (headline (org-element-property :raw-value element))
               (contents-begin (org-element-property :contents-begin element))
               (contents-end   (org-element-property :contents-end element))
               (content (if (and contents-begin contents-end)
                            (buffer-substring-no-properties contents-begin
                                                            contents-end)))
               (clipping
                (org-export-string-as content 'gfm t '(:with-toc nil)))
               (payload (string-join (list headline clipping) "\n")))

          (kill-new payload)
          (fj-request-issue-create repo)))

    (cond
     ((not (derived-mode-p 'org-mode))
      (message "This command only supported in an `org-mode' buffer"))

     ((not fj-username)
      (error "The variable ‘fj-username’ must be set to the GitHub user name"))
     (t
      (error "undefined condition")))))


;;; Transients

(transient-define-prefix fj-issues-tmenu ()
  "GitHub issues client menu."
  ["GitHub Issues"
   :description (lambda () (format "GitHub Issues: %s" fj-repo-name))
   ["Actions"
    :pad-keys t
    ("RET" "Browse" fj-browse-issue)
    ("c" "Copy as Org" fj-copy-issue)
    ("K" "Close all opened issues" fj-kill-all-repo-buffers)
    ("N" "New Issue…" fj-request-issue-create)]

   ["Navigation"
    ("p" "↑" previous-line :transient t)
    ("n" "↓" next-line :transient t)]

   ["View"
    ("g" "Refresh" vtable-revert-command)
    ("b" "Browse URL" fj-browse-url)
    ("t" "Toggle Truncate Lines" toggle-truncate-lines)]]

  [:class transient-row
   ("f" "Change Repo…" fj)
   ("Q" "Quit" View-kill-and-leave)])

(provide 'fj)
;;; fj.el ends here
