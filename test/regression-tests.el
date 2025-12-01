;;; regression-tests.el --- basic regression tests for notmuch-dashboard.el

;;; Commentary:

;; These tests will load the two sample dashboards, activate them, and
;; assure basic functionality.

;;; Code:

(require 'ert)
(require 'notmuch-dashboard)
(require 'cl-lib)

(setq-default
 notmuch-dashboard-notmuch-program
 (format "%s/test/mock-notmuch-cli" (getenv "srcdir")))

(ert-deftest smoke-tests ()
  "Simple notmuch-dashboard test."
  (find-file (format "%s/test/trivial-dash.org" (getenv "srcdir")))
  (notmuch-dashboard-mode)

  (while (< (buffer-chars-modified-tick) 8)
    (sit-for 1)
    (while (accept-process-output)))

  (should
   (string-equal
    (buffer-string)
    "
* Mailboxes                 *[[nm:tag:unread|%2d][17]]*

[[nm:tag:unread][Unread]] /[[nm:tag:unread|(%3d)][( 17)]]/ .... [u]  [[nm:date:today..now][Today]] /[[nm:date:today..now|(%3d)][(113)]]/ .......... [t]  *Compose* ...... [C]

#+KEYMAP: u | notmuch-search \"tag:unread\"
#+KEYMAP: t | notmuch-search \"date:today..now\"
#+KEYMAP: C | notmuch-mua-new-mail
"))

  (should
   (string-match-p
    (regexp-quote "notmuch-mua-new-mail")
    (prin1-to-string (lookup-key (current-local-map) "C")))))

(ert-deftest bookmark-expansion-test ()
  "Ensure saved searches are expanded for counts."
  (let ((notmuch-saved-searches '((:name "Unread" :query "tag:unread"))))
    (find-file (format "%s/test/trivial-dash.org" (getenv "srcdir")))
    (notmuch-dashboard-mode)
    (let* ((link (car (org-element-map (org-element-parse-buffer) 'link #'identity)))
           (path (org-element-property :path link)))
      (should (string-match "tag:unread" path))
      (notmuch-dashboard-update-link link)
      (goto-char (org-element-property :contents-begin link))
      (should (looking-at "17")))))

(ert-deftest limited-query-uses-threads ()
  "Verify limited queries honor the requested thread cap."
  (let (limited-query)
    (cl-letf* (((symbol-function 'notmuch-search)
                (lambda (query &rest _ignored)
                  (setq limited-query query))))
      (let* ((limit "3")
             (query "tag:unread")
             (path (format "%s|%%2d|%s" query limit)))
        (notmuch-dashboard-follow-notmuch-link path)
        (should (string-match "thread:mock-0" limited-query))
        (should (string-match "thread:mock-1" limited-query))
        (should (string-match "thread:mock-2" limited-query))))))

(provide 'regression-tests)
;;; regression-tests.el ends here
