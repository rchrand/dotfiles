;;; structural-editing.el --- Tree-sitter-aware editing -*- lexical-binding: t; -*-

;; Combobulate extends standard Emacs structural movement.  Keep it scoped to
;; supported tree-sitter modes; C intentionally remains plain `c-mode'.
(defvar combobulate-key-prefix)
(defvar combobulate-tsx-highlight-queries-default)

;; This must be set before Straight loads Combobulate's generated autoloads.
;; `C-c o' belongs to Crux (`crux-open-with').
(setq combobulate-key-prefix "C-c n")

(defun rchrand/combobulate-setup ()
  "Enable Combobulate after its compatibility settings are applied."
  (require 'combobulate)
  (combobulate-mode))

(use-package combobulate
  :straight (:type git :host github :repo "mickeynp/combobulate")
  :commands combobulate-mode
  :hook ((typescript-ts-mode . rchrand/combobulate-setup)
         (tsx-ts-mode . rchrand/combobulate-setup)
         (python-ts-mode . rchrand/combobulate-setup)
         (yaml-ts-mode . rchrand/combobulate-setup)
         (json-ts-mode . rchrand/combobulate-setup)
         (toml-ts-mode . rchrand/combobulate-setup))
  :config
  ;; The installed Emacs 31 TSX grammar is newer than Combobulate's optional
  ;; cosmetic highlight queries.  Navigation and editing remain compatible.
  (setq combobulate-tsx-highlight-queries-default nil))

(provide 'structural-editing)
;;; structural-editing.el ends here
