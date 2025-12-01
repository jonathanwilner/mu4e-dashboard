;;; notmuch-dashboard.el --- Dashboards for notmuch   -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/notmuch-dashboard
;; Keywords: convenience
;; Version: 0.2

;; Package-Requires: ((emacs "27.1") (notmuch) (async) (org))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; notmuch-dashboard provides enhanced org-mode links that allow you to
;; define custom dashboards that link back to the notmuch email client.
;;

;;; Code:
(require 'subr-x)
(require 'ob-shell)
(require 'org)
(require 'org-element)
(require 'async)
(require 'cl-lib)
(require 'notmuch)

(defconst notmuch-dashboard-version "0.1.1")

;; Install the notmuch link type
(defgroup notmuch-dashboard nil
  "Provides a new Org mode link type for notmuch queries."
  :group 'comm)

(defcustom notmuch-dashboard-file "~/.emacs.d/notmuch-dashboard.org"
  "Path to the dashboard org file."
  :type 'string)

(defcustom notmuch-dashboard-link-name "nm"
  "Default link name."
  :type 'string)

(defcustom notmuch-dashboard-notmuch-program "notmuch"
  "Default name of the notmuch command."
  :type 'string)

(defcustom notmuch-dashboard-lighter " nmdsh"
  "Minor mode lighter indicating that this mode is active."
  :type 'string)

(defcustom notmuch-dashboard-propagate-keymap t
  "Propagate dashboard defined keymap to notmuch header view."
  :type 'boolean)

(org-link-set-parameters
 notmuch-dashboard-link-name
 :follow #'notmuch-dashboard-follow-notmuch-link)

(defvar notmuch-dashboard--prev-local-keymap nil
  "Buffer-local variable to save the prior keymap.")

(make-variable-buffer-local 'notmuch-dashboard--prev-local-keymap)

(defvar notmuch-dashboard--async-update-in-progress nil
  "Set to t if an async update is in progress.

This is a buffer-local variable that will be t if the current
buffer is in the process of being updated asynchronously.")

(make-variable-buffer-local 'notmuch-dashboard--async-update-in-progress)

;;;###autoload
(define-minor-mode notmuch-dashboard-mode
  "Minor mode for \"live\" notmuch dashboards."
  :lighter notmuch-dashboard-lighter
  :init-value nil
  (if notmuch-dashboard-mode
      (progn
        (setq buffer-read-only t)
        ;; Make a copy of the current local keymap (this will, in
        ;; general, have been setup by org-mode, but I don't want to
        ;; assume that)
        (setq notmuch-dashboard--prev-local-keymap (current-local-map))
        (use-local-map (make-composed-keymap (notmuch-dashboard-parse-keymap) (current-local-map)))
        ;; If buffer corresponds to the dashboard, add a special key
        ;; (buffer-name is harcoded). Dashboard should be open with a
        ;; special function naming a defcustom buffer name and then
        ;; install the minor mode.  install the keymap as local with
        ;; current map as parent (this might generate some problem?)
        (if (string= (buffer-file-name) (expand-file-name notmuch-dashboard-file))
            (local-set-key (kbd "<return>") #'org-open-at-point))
        (add-hook 'notmuch-hello-refresh-hook #'notmuch-dashboard-update)
        (when notmuch-dashboard-propagate-keymap
          ;; install minor mode to notmuch search view when called
          (add-hook 'notmuch-search-hook #'notmuch-dashboard-mode))
        (notmuch-dashboard-update))
    (if notmuch-dashboard--async-update-in-progress
        (user-error "Update in progress; try again when it is complete"))
    (remove-hook 'notmuch-hello-refresh-hook #'notmuch-dashboard-update)
    ;; clear hook when dashboard disable
    (remove-hook 'notmuch-search-hook #'notmuch-dashboard-mode)
    (use-local-map notmuch-dashboard--prev-local-keymap)
    (setq buffer-read-only nil)))

(defun notmuch-dashboard ()
  "Open the dashboard (using notmuch-dashboard-file)."
  (interactive)
  (if (file-exists-p notmuch-dashboard-file)
      (progn
        (find-file notmuch-dashboard-file)
        (notmuch-dashboard-mode))
    (message (concat notmuch-dashboard-file " does not exist"))))

(defun notmuch-dashboard-compare-saved-search (saved-search field-name st)
  "Compare ST to the field FIELD-NAME in the SAVED-SEARCH."
  (equal (plist-get saved-search field-name) st))

(defun notmuch-dashboard-find-saved-search (name)
  "Convert a notmuch saved search NAME to a query."
  (when name
    (cl-find-if (lambda (a) (notmuch-dashboard-compare-saved-search a :name name))
                notmuch-saved-searches)))

(defun notmuch-dashboard-translate-saved-search-to-query (bm)
  "Translates a BM:<bookmarkName> into a notmuch query."
  (let ((saved-search (notmuch-dashboard-find-saved-search (substring bm 3))))
    (if saved-search
        ;; put parenthesis around to make it hygienic
        (format "(%s)" (plist-get saved-search :query))
      (progn
        (message (concat "saved search not found: " bm))
        bm))))

(defun notmuch-dashboard-expand-saved-searches-in-query (st)
  "Replace ST by the corresponding query if ST contain a bookmark."
  (let ((bookmark-re "\\(bm:[^ ]+\\)"))
    (replace-regexp-in-string bookmark-re #'notmuch-dashboard-translate-saved-search-to-query st)))

(defun notmuch-dashboard--sanitize-query (query)
  "Strip wrapping quotes from QUERY to ease comparisons."
  (replace-regexp-in-string "^'\|\'$" "" query))

(defun notmuch-dashboard--limited-thread-query (query count)
  "Return a query limited to COUNT threads based on QUERY.
If COUNT cannot be parsed or no threads are found, return QUERY."
  (let* ((limit (string-to-number count))
         (expanded-query (notmuch-dashboard-expand-saved-searches-in-query query))
         (clean-query (notmuch-dashboard--sanitize-query expanded-query))
         (command (format "%s search --format=text --output=threads --limit=%d %s 2> /dev/null"
                          notmuch-dashboard-notmuch-program
                          limit
                          (shell-quote-argument clean-query)))
         (raw-output (and (> limit 0) (shell-command-to-string command)))
         (threads (and raw-output (split-string raw-output "\n" t "[[:space:]]+"))))
    (if (and threads (> (length threads) 0))
        (mapconcat (lambda (thread) (format "%s" thread)) threads " or ")
      clean-query)))

(defun notmuch-dashboard--open-search (query)
  "Open a notmuch search buffer for QUERY."
  (when (get-buffer-window "*notmuch-search*" t)
    (switch-to-buffer "*notmuch-search*"))
  (notmuch-search query))

(defun notmuch-dashboard-follow-notmuch-link (path)
  "Process a notmuch link with path PATH.

PATH shall be of the form [[nm:query|fmt|limit][(---------)]].
If FMT is not specified or is nil, clicking on the link calls
notmuch with the specified QUERY (with or without the given
LIMIT).  If FMT is specified, the description of the link is
updated with the QUERY count formatted using the provided
format (for example \"%4d\")."

  (let* ((link (org-element-context))
         (queryname (string-trim (nth 0 (split-string path "[]|]"))))
         (query (notmuch-dashboard-expand-saved-searches-in-query queryname))
         (fmt (nth 1 (split-string path "[]|]")))
         (count (nth 2 (split-string path "[]|]"))))
    (cond
     ;; Regular query without limit
     ((and (not fmt) (not count))
      (notmuch-dashboard--open-search query))

     ;; Regular query with limit
     ((and count (> (length count) 0))
      (let ((limited-query (notmuch-dashboard--limited-thread-query query count)))
        (notmuch-dashboard--open-search limited-query)))

     ;; Query count and link description update
     ((and fmt (> (length fmt) 0))
      (notmuch-dashboard-update-link link)))))

(defun notmuch-dashboard-update-link (link)
  "Update content of a formatted notmuch LINK."

;; A formatted link is a link of the form
;; [[nm:query|limit|fmt][(---------)]] where fmt is a non nil
;; string describing the format.  When a link is cleared, the
;;     description is replaced by a string for the form "(---)" and
;; have the same size as the current description. If the given
;; format is too big for the current description, description is
;; replaced with + signs.

  (let* ((path (org-element-property :path link))
         (queryname (string-trim (nth 0 (split-string path "|"))))
         (query (notmuch-dashboard-expand-saved-searches-in-query queryname))
         (fmt (nth 1 (split-string path "|")))
         (beg (org-element-property :contents-begin link))
         (end (org-element-property :contents-end link))
         (size (- end beg)))
    (when (and fmt (> (length fmt) 0))
      (let* ((command (format "%s count %s 2> /dev/null"
                              notmuch-dashboard-notmuch-program
                              (shell-quote-argument query)))
             (output (string-to-number (shell-command-to-string command)))
             (output (format fmt output)))
        (let ((modified (buffer-modified-p))
              (inhibit-read-only t))
          (save-excursion
            (delete-region beg end)
            (goto-char beg)
            (insert (if (<= (length output) size)
                        output
                      (make-string size ?+))))
          (set-buffer-modified-p modified))))))

(defun notmuch-dashboard-update-all-async ()
  "Update content of all formatted notmuch links asynchronously."

;; A formatted link is a link of the form
;; [[nm:query|limit|fmt][(---------)]] where fmt is a non nil
;; string describing the format.  When a link is cleared, the
;; description is replaced by a string for the form "(---)" and
;; have the same size as the current description.

  (when notmuch-dashboard--async-update-in-progress
    (user-error "Cannot update while an update is in progress!"))
  (setq notmuch-dashboard--async-update-in-progress t)
  (let ((buffer (current-buffer)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) notmuch-dashboard-link-name)
          (let* ((path (org-element-property :path link))
                 (query (string-trim (nth 0 (split-string path "|"))))
                 (fmt (nth 1 (split-string path "|")))
                 (beg (org-element-property :contents-begin link))
                 (end (org-element-property :contents-end link))
                 (size (if (and beg end) (- end beg) 0)))
            (when (and fmt (> (length fmt) 0))
              (when (eq size 0)
                (error "The link ``%s'' has a format clause, but no output width" path))

              (async-start
               (lambda ()
                 (let ((command (format "%s count %s 2> /dev/null"
                                        notmuch-dashboard-notmuch-program
                                        (shell-quote-argument
                                         (notmuch-dashboard-expand-saved-searches-in-query query)))))
                   (string-to-number (shell-command-to-string command))))
               (lambda (count)
                 (with-current-buffer buffer
                   (let ((modified (buffer-modified-p))
                         (inhibit-read-only t)
                         (output (if (numberp count)
                                     (format fmt count)
                                   (format fmt 0))))
                     (save-excursion
                       (delete-region beg end)
                       (goto-char beg)
                       (insert (if (<= (length output) size) output
                                 (make-string size ?+))))
                     (set-buffer-modified-p modified)))))))))))
  (setq notmuch-dashboard--async-update-in-progress nil))

(defun notmuch-dashboard-update-all-sync ()
  "Update content of all notmuch formatted links in a synchronous way.

A formatted link is a link of the form
[[nm:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (notmuch-dashboard-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) notmuch-dashboard-link-name)
        (notmuch-dashboard-update-link link)
        (redisplay t)))))

(defun notmuch-dashboard-clear-link (link)
  "Clear a formatted notmuch link LINK.

A formatted link is a link of the form
[[nm:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
having the same size as the current description."

  (let* ((path (org-element-property :path link))
         (fmt (nth 1 (split-string path "|")))
         (beg (org-element-property :contents-begin link))
         (end (org-element-property :contents-end link))
         (size (- end beg)))
    (when (and fmt (> (length fmt) 0))
      (let ((modified (buffer-modified-p))
            (inhibit-read-only t))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (format "(%s)" (make-string (- size 2) ?-))))
        (set-buffer-modified-p modified)))))

(defun notmuch-dashboard-clear-all ()
  "Clear all formatted notmuch links.

A formatted link is a link of the form
[[nm:query|limit|fmt][(---------)]] where fmt is a non nil
string describing the format.  When a link is cleared, the
description is replaced by a string for the form \"(---)\" and
have the same size as the current description."

  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) notmuch-dashboard-link-name)
        (notmuch-dashboard-clear-link link))))
  (redisplay t))

(defun notmuch-dashboard-update ()
  "Update the current dashboard."
  (interactive)
  (dolist (buffer (buffer-list (current-buffer)))
      (with-current-buffer buffer
        (when (bound-and-true-p notmuch-dashboard-mode)
          (if buffer-read-only
              (notmuch-dashboard-update-all-sync)
            (error "Dashboard cannot be updated only in read-only mode"))))))

(defun notmuch-dashboard-parse-keymap ()
  "Parse the current buffer file for keybindings.

Keybindings are defined by keywords of type KEYMAP:VALUE and
install the corresponding key bindings in the notmuch-dashboard
minor mode keymap.  The previous keymap (if any) is erased.

VALUE is composed of \"keybinding | function-call\" with
keybinding begin a string describing a key sequence and a call to
an existing function.  For example, to have \"q\" to kill the
current buffer, the syntax would be:

#+KEYMAP: q | kill-current-buffer

This can be placed anywhere in the org file even though I advised
to group keymaps at the same place."

  (let ((map (make-sparse-keymap)))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
        (when (string= (org-element-property :key keyword) "KEYMAP")
          (let* ((value (org-element-property :value keyword))
                 (key (string-trim (nth 0 (split-string value "|"))))
                 (call (string-trim (nth 1 (split-string value "|")))))
            (define-key map
              (kbd key)
              `(lambda () (interactive) ,(car (read-from-string (format "(%s)" call)))))
            (message "notmuch-dashboard: binding %s to %s"
                     key
                     (format "(lambda () (interactive) (%s))" call))))))
    map))

(provide 'notmuch-dashboard)
;;; notmuch-dashboard.el ends here
