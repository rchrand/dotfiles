;;; common-lisp-config.el --- SBCL, SLY, and Qlot -*- lexical-binding: t; -*-

;; SLY is installed through Straight. Qlot manages project dependencies.

(defun rchrand/sly-start ()
  "Start SLY with Qlot in a project containing qlfile, otherwise plain SBCL."
  (interactive)
  (unless (executable-find "sbcl")
    (user-error "SBCL is unavailable; install it with: brew install sbcl"))
  (let* ((root (locate-dominating-file default-directory "qlfile"))
         (default-directory (or root default-directory)))
    (when (and root (not (executable-find "qlot")))
      (user-error "This project needs Qlot, but qlot is not on Emacs's PATH"))
    (sly (if root 'qlot 'sbcl))))

(use-package sly
  :commands sly
  :bind ("C-c l s" . rchrand/sly-start)
  :init
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "sbcl" "--noinform")
                :coding-system utf-8-unix))))

(use-package lisp-mode
  :straight nil
  :mode ("\\.asd\\'" . lisp-mode)
  :hook (lisp-mode . eldoc-mode))

;; Previous SLIME setup: uncomment this block and disable SLY above to revert.
;; (defconst rchrand/quicklisp-setup-files
;;   (mapcar (lambda (path) (expand-file-name path (getenv "HOME")))
;;           '(".quicklisp/setup.lisp" "quicklisp/setup.lisp"))
;;   "Standard Quicklisp setup file locations, newest convention first.")
;;
;; (defun rchrand/quicklisp-setup-file ()
;;   "Return the installed Quicklisp setup file, or nil when absent."
;;   (catch 'setup-file
;;     (dolist (file rchrand/quicklisp-setup-files)
;;       (when (file-exists-p file)
;;         (throw 'setup-file file)))))
;;
;; (defun rchrand/slime-start ()
;;   "Start SBCL through SLIME, with an actionable error when it is unavailable."
;;   (interactive)
;;   (unless (executable-find "sbcl")
;;     (user-error "SBCL is unavailable; install it with: brew install sbcl"))
;;   (unless (rchrand/quicklisp-setup-file)
;;     (message "Quicklisp is not installed; SLIME will start, but ,ql is unavailable"))
;;   (slime))
;;
;; (use-package slime
;;   :commands slime
;;   :bind ("C-c l s" . rchrand/slime-start)
;;   :init
;;   ;; Keep the banner out of the REPL but otherwise let SBCL load its normal
;;   ;; user init file, which is where Quicklisp installs its startup hook.
;;   (setq inferior-lisp-program
;;         (concat (or (executable-find "sbcl") "sbcl") " --noinform")
;;         slime-contribs '(slime-fancy slime-asdf slime-quicklisp)))
;;

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
