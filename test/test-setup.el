;;; test-setup.el --- ensure dependencies for notmuch-dashboard tests -*- lexical-binding: t; -*-
(require 'package)
(setq package-user-dir (expand-file-name ".packages" (getenv "srcdir")))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(async notmuch))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(provide 'test-setup)
;;; test-setup.el ends here
