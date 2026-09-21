;;; completion.el --- Minibuffer completion and search -*- lexical-binding: t; -*-

(use-package savehist
  :straight nil
  :init
  (setq history-length 500
        savehist-file (expand-file-name "history.eld" rchrand-savefile-dir))
  :config
  (savehist-mode 1))

(use-package vertico
  :init
  (setq vertico-cycle t
        vertico-count 15)
  :config
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(defun rchrand/consult-line-at-point ()
  "Search the current buffer, starting with the symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol t)))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-r" . consult-line)
         ("M-s s" . isearch-forward)
         ("C-S-s" . rchrand/consult-line-at-point)
         ;; Preserve ordinary `yank-pop' immediately after a yank; otherwise
         ;; use Consult's searchable kill-ring picker with live preview.
         ("M-y" . consult-yank-pop)
         ("C-c /" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("C-x j" . consult-find)
         ("C-x C-j" . dired-jump)
         ("C-x C-," . consult-global-mark)
         ("C-c i" . consult-imenu))
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize consult-ripgrep consult-git-grep consult-grep
                     :preview-key '(:debounce 0.2 any)))

(use-package embark
  :bind (("C-;" . embark-act)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-collect)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completion)
;;; completion.el ends here
