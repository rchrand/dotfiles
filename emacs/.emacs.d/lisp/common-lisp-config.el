;;; common-lisp-config.el --- SBCL, SLIME, and Quicklisp -*- lexical-binding: t; -*-

;; SLIME is versioned by Straight along with the rest of this configuration.
;; Quicklisp remains the Common Lisp-side library manager and is loaded by
;; SBCL's user init file after its normal one-time installation.

(defconst rchrand/quicklisp-setup-files
  (mapcar (lambda (path) (expand-file-name path (getenv "HOME")))
          '(".quicklisp/setup.lisp" "quicklisp/setup.lisp"))
  "Standard Quicklisp setup file locations, newest convention first.")

(defun rchrand/quicklisp-setup-file ()
  "Return the installed Quicklisp setup file, or nil when absent."
  (catch 'setup-file
    (dolist (file rchrand/quicklisp-setup-files)
      (when (file-exists-p file)
        (throw 'setup-file file)))))

(defun rchrand/slime-start ()
  "Start SBCL through SLIME, with an actionable error when it is unavailable."
  (interactive)
  (unless (executable-find "sbcl")
    (user-error "SBCL is unavailable; install it with: brew install sbcl"))
  (unless (rchrand/quicklisp-setup-file)
    (message "Quicklisp is not installed; SLIME will start, but ,ql is unavailable"))
  (slime))

(use-package lisp-mode
  :straight nil
  :mode ("\\.asd\\'" . lisp-mode)
  :hook (lisp-mode . eldoc-mode))

(use-package slime
  :commands slime
  :bind ("C-c l s" . rchrand/slime-start)
  :init
  ;; Keep the banner out of the REPL but otherwise let SBCL load its normal
  ;; user init file, which is where Quicklisp installs its startup hook.
  (setq inferior-lisp-program
        (concat (or (executable-find "sbcl") "sbcl") " --noinform")
        slime-contribs '(slime-fancy slime-asdf slime-quicklisp)))

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
