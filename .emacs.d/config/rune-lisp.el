;;; package --- Summary
;;; Commentary:
;;;;;;;;;;;;;;; Emacs Lisp
(global-set-key (kbd "C-h K") 'find-function-on-key)

;;;;;;;;;;;;;;;  Clojure:
(require 'cider)
(require 'clojure-mode)

;;;;;;;;;;;;;;;  Scheme
(require 'xscheme)
(setq scheme-program-name "guile")

;;;;;;;;;;;;;; Common Lisp
(require 'slime)
(setq inferior-lisp-program "ccl")
(slime-setup '(slime-fancy))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	   slime-fuzzy-completion-in-place t
	   slime-enable-evaluate-in-emacs t
	   slime-autodoc-use-multiline-p t)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

;; Hooks

(defun enable-lisp-utils ()
  (turn-on-eldoc-mode)
  (auto-complete-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (require 'evil-paredit)
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (evil-paredit-mode t))

(add-hook 'lisp-mode-hook 'enable-lisp-utils)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-lisp-utils)
(add-hook 'clojure-mode-hook 'enable-lisp-utils)

;; Function to evaluate region if active
(defun rune/eval-last-sexp-or-region (beg end prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "r\nP")
  (if (use-region-p)
      (eval-region beg end)
    (pp-eval-last-sexp prefix)))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(after-load 'lisp-mode
	    (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'rune/eval-last-sexp-or-region))

(after-load 'paredit
  (diminish 'paredit-mode "Par"))

(provide 'rune-lisp)
;;; rune-lisp.el ends here
