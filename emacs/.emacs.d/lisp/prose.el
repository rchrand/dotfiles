;;; prose.el --- Focused Org and prose editing -*- lexical-binding: t; -*-

(use-package visual-fill-column
  :straight (visual-fill-column
             :type git
             :host nil
             :repo "https://codeberg.org/joostkremers/visual-fill-column.git")
  :commands visual-fill-column-mode)

(defun rchrand/prose-mode-setup ()
  "Make the current text buffer comfortable for sustained writing."
  (visual-line-mode 1)
  (display-line-numbers-mode -1)
  (setq-local truncate-lines nil
              word-wrap t
              fill-column 80
              visual-fill-column-width 80
              visual-fill-column-center-text nil
              sentence-end-double-space nil)
  (visual-fill-column-mode 1))

;; Run after the general UI hook so prose wrapping wins over code defaults.
(add-hook 'text-mode-hook #'rchrand/prose-mode-setup t)

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(when-let ((aspell (executable-find "aspell")))
  (setq ispell-program-name aspell))

(global-set-key (kbd "C-c s") #'ispell-word)

(save-place-mode 1)

(use-package org
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-c C-o" . org-open-at-point))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-fold-catch-invisible-edits 'show-and-error
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-return-follows-link t
        org-startup-folded 'content
        org-startup-indented t
        org-use-speed-commands t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

;; Make config reloads update prose buffers that are already open.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'text-mode)
      (rchrand/prose-mode-setup))))

(provide 'prose)
;;; prose.el ends here
