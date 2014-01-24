(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'linum-relative)
(global-linum-mode)
   
(global-set-key (kbd "C-c k")
                (lambda ()
                  (interactive)
                  (if (equal t global-linum-mode)
                      (global-linum-mode 0)
                    (global-linum-mode 1))))


(require 'grizzl)
(require 'fiplr)
(setq fiplr-root-markers '(".git" ".svn")) ;; Root folders (Could be customized to some JS'projects)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key (kbd "C-c p") 'fiplr-find-file)

(provide 'rune-editor)
