;;; package --- Summary
;;; Commentary:
(add-to-list 'load-path "~/.emacs.d/vendor/")

;;;;;;;;;;;;;;; Emacs Lisp

(defun my-emacs-lisp-hook ()
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-hook 'my-emacs-lisp-hook)

;;;;;;;;;;;;;;;  Clojure:
(require 'cider)
(require 'clojure-mode)

;;;;;;;;;;;;;;;  Scheme
(require 'xscheme)
(setq scheme-program-name "guile")

;;;;;;;;;;;;;; Common Lisp
(require 'slime)
(setq inferior-lisp-program "sbcl")
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

(defun enable-lisp-utils ()
  (auto-complete-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (require 'evil-paredit)
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (evil-paredit-mode t))

; Misc
(add-hook 'lisp-mode-hook 'enable-lisp-utils)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(provide 'rune-lisp)
;;; rune-lisp.el ends here
