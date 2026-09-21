;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-

(use-package org
  :straight nil
  :defer nil
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :config
  (setq org-agenda-files (list org-directory)
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-fold-catch-invisible-edits 'show-and-error
        org-hide-emphasis-markers t
        org-fast-tag-selection-single-key t
        org-reverse-note-order t
        org-use-fast-todo-selection t
        org-priority-highest ?A
        org-priority-lowest ?Z
        org-priority-default ?C
        org-agenda-deadline-leaders '("" "" "%2d d. ago: ")
        org-deadline-warning-days 0
        org-agenda-span 7
        org-agenda-start-day "-0d"
        org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done)
        org-todo-keywords '((sequence "STARTED(s)" "WAITING(w)" "TODO(t)" "|" "DONE(d)")
                            (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)"))
        org-todo-keyword-faces '(("STARTED" . (:foreground "systemYellowColor" :weight bold))
                                  ("DONE" . (:foreground "ForestGreen" :weight bold))
                                  ("WAITING" . (:foreground "MediumPurple2" :weight bold))
                                  ("CANCELED" . (:foreground "systemRedColor" :weight bold))
                                  ("DELEGATED" . (:foreground "DeepSkyBlue" :weight bold))
                                  ("SOMEDAY" . (:foreground "PaleVioletRed2" :weight bold)))
        org-capture-templates `(("t" "Todo" entry (file ,(expand-file-name "todo.org" org-directory))
                                 "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
                                ("i" "inbox" entry (file ,(expand-file-name "inbox.org" org-directory))
                                 "* %?"))
        org-todo-sort-order '("STARTED" "WAITING" "TODO" "DELEGATED" "CANCELED" "DONE")))

(defun my/org-custom-todo-sort-key ()
  (let* ((my-order '("STARTED" "WAITING" "TODO" "DELEGATED" "CANCELED" "DONE"))
         (todo (org-get-todo-state)))
    (or (cl-position todo my-order :test #'equal)
        (length my-order))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

(provide 'org-config)
;;; org-config.el ends here
