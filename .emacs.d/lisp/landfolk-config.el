;;; landfolk-config.el --- Landfolk monorepo workflow -*- lexical-binding: t; -*-

(require 'subr-x)

(declare-function lsp-workspace-folders-add "lsp-mode" (project-root))
(defvar lsp-clients-typescript-prefer-use-project-ts-server)
(defvar lsp-javascript-format-enable)
(defvar lsp-typescript-format-enable)

(defconst rchrand/landfolk-root (expand-file-name "~/landfolk/")
  "Root of the local Landfolk checkout.")

(defun rchrand/landfolk-node-tool (program)
  "Return Landfolk's repository-local PROGRAM, or a PATH fallback."
  (let ((local-program
         (expand-file-name (concat "node_modules/.bin/" program)
                           rchrand/landfolk-root)))
    (if (file-executable-p local-program)
        local-program
      (executable-find program))))

(defun rchrand/landfolk-buffer-p ()
  "Return non-nil when the current buffer belongs to the Landfolk checkout."
  (when-let* ((file (or buffer-file-name default-directory)))
    (file-in-directory-p (file-truename file)
                         (file-truename rchrand/landfolk-root))))

(defun rchrand/landfolk-add-lsp-folder (directory)
  "Register DIRECTORY as a distinct LSP workspace when it exists."
  (when (file-directory-p directory)
    (require 'lsp-mode)
    (lsp-workspace-folders-add directory)))

(defun rchrand/typescript-mode-dwim ()
  "Use tree-sitter TypeScript when available, with a built-in fallback."
  (if (and (fboundp 'typescript-ts-mode)
           (treesit-ready-p 'typescript t))
      (typescript-ts-mode)
    (js-mode)))

(defun rchrand/tsx-mode-dwim ()
  "Use tree-sitter TSX when available, with a built-in fallback."
  (if (and (fboundp 'tsx-ts-mode)
           (treesit-ready-p 'tsx t))
      (tsx-ts-mode)
    (js-jsx-mode)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . rchrand/typescript-mode-dwim))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rchrand/tsx-mode-dwim))

(use-package reformatter)

(reformatter-define prettier-format
  :program (or (rchrand/landfolk-node-tool "prettier") "prettier")
  :args (list "--stdin-filepath" (buffer-file-name))
  :group 'rchrand-formatters
  :lighter " Prettier")

(defun rchrand/landfolk-typescript-setup ()
  "Enable Landfolk TypeScript code intelligence."
  (when (rchrand/landfolk-buffer-p)
    (setq-local lsp-clients-typescript-prefer-use-project-ts-server t)
    (when (rchrand/landfolk-node-tool "prettier")
      (prettier-format-on-save-mode 1))
    (lsp-deferred)))

(dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook
                js-mode-hook js-jsx-mode-hook))
  (add-hook hook #'rchrand/landfolk-typescript-setup))

(with-eval-after-load 'lsp-javascript
  (setq lsp-clients-typescript-prefer-use-project-ts-server t
        lsp-javascript-format-enable nil
        lsp-typescript-format-enable nil))

(defun rchrand/landfolk-rubocop-buffer ()
  "Save and autocorrect the current Landfolk Ruby file with RuboCop."
  (interactive)
  (unless (and buffer-file-name (executable-find "bundle"))
    (user-error "Bundler is not available in this buffer's environment"))
  (save-buffer)
  (let ((output (get-buffer-create "*rubocop-autocorrect*")))
    (with-current-buffer output
      (erase-buffer))
    (if (eq 0 (process-file "bundle" nil output nil
                            "exec" "rubocop" "--autocorrect-all"
                            buffer-file-name))
        (progn
          (revert-buffer nil t)
          (message "RuboCop formatted %s" (file-name-nondirectory buffer-file-name)))
      (display-buffer output)
      (user-error "RuboCop failed; see *rubocop-autocorrect*"))))

(defun rchrand/landfolk-ruby-setup ()
  "Enable Sorbet LSP for Ruby files in Landfolk."
  (when (and (rchrand/landfolk-buffer-p)
             (locate-dominating-file default-directory "sorbet/config"))
    (require 'lsp-sorbet)
    (setq-local lsp-sorbet-use-bundler t)
    (when (boundp 'flycheck-ruby-rubocop-executable)
      (setq-local flycheck-ruby-rubocop-executable "rubocop"))
    (local-set-key (kbd "C-c f") #'rchrand/landfolk-rubocop-buffer)
    (rchrand/landfolk-add-lsp-folder
     (expand-file-name "apps/api/" rchrand/landfolk-root))
    (lsp-deferred)))

(add-hook 'ruby-mode-hook #'rchrand/landfolk-ruby-setup)
(add-hook 'ruby-ts-mode-hook #'rchrand/landfolk-ruby-setup)

(defun rchrand/landfolk-python-root-setup ()
  "Keep Landfolk data analysis in its own LSP workspace."
  (when (rchrand/landfolk-buffer-p)
    (rchrand/landfolk-add-lsp-folder
     (expand-file-name "data/" rchrand/landfolk-root))))

(add-hook 'python-mode-hook #'rchrand/landfolk-python-root-setup)
(add-hook 'python-ts-mode-hook #'rchrand/landfolk-python-root-setup)

(with-eval-after-load 'projectile
  (when (file-directory-p rchrand/landfolk-root)
    (projectile-add-known-project rchrand/landfolk-root)))

(provide 'landfolk-config)
;;; landfolk-config.el ends here
