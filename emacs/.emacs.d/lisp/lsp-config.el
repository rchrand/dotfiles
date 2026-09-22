;;; lsp-config.el --- Shared LSP and completion setup -*- lexical-binding: t; -*-

(defgroup rchrand-formatters nil
  "Formatting commands used by the personal Emacs configuration."
  :group 'tools)

;; Tree-sitter auto install
(use-package treesit-auto
  :when (fboundp 'treesit-available-p)
  :init
  (setq treesit-auto-install 'prompt
        treesit-auto-langs '(bash c json python ruby toml tsx typescript yaml))
  :config (global-treesit-auto-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c C-l"
        ;; Load only clients used by active modules.  Besides faster startup,
        ;; this avoids unrelated clients breaking LSP on development Emacs.
        lsp-client-packages '(lsp-javascript lsp-pyright lsp-sorbet lsp-toml))
  :hook ((toml-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (define-key prog-mode-map (kbd "C-c C-l") lsp-command-map)
  (setq lsp-completion-provider :none
        lsp-headerline-breadcrumb-enable t
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-eldoc-enable-hover t
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-enable t
        lsp-enable-snippet t
        lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :after lsp-mode
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t)
  :bind (:map lsp-ui-mode-map
              ("C-c C-l ." . lsp-ui-peek-find-definitions)
              ("C-c C-l ?" . lsp-ui-peek-find-references)
              ("C-c C-l i" . lsp-ui-imenu)))

(use-package company
  :diminish
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-quick-access t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (add-to-list 'company-backends 'company-capf)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.3
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode 'right-fringe))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package consult-lsp
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
              ("C-c C-l s" . consult-lsp-symbols)
              ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package treemacs
  :defer t)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols))

(provide 'lsp-config)
;;; lsp-config.el ends here
