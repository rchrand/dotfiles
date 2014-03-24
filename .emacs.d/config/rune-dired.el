(require 'dired)
(require 'dired+)

(setq direp-hide-details-initially-flag nil)
(put 'dired-find-alternate-file 'disabled nil)

(after-load 'dired
  (require 'dired+)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(provide 'rune-dired)
