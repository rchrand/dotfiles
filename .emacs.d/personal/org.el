;;; package -- Summary
;;; Commentary:
;;; Code:
(setq org-log-done nil)

(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)
(setq org-startup-folded t)

;; Org-agenda
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-compact-blocks t)
(setq org-agenda-dim-blocked-tasks nil)

(setq org-todo-keywords '((sequence "TODO(t)" "NOTE(o)" "NEXT(n)" "MEETING(m)"
                                    "PHONE(p)" "CANCELLED(c)" "IF TIME(i)" "|" "DONE(d)")))

(setq org-agenda-window-setup 'current-window)

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NOTE" . "DeepSkyBlue4")
        ("NEXT" . "chocolate4")
        ("MEETING" . "gold1")
        ("PHONE" . "PaleGreen4")
        ("CANCELLED" . "firebrick")
        ("IF TIME" . "OrangeRed")
        ("DONE" . "green")))

(setq org-agenda-files
      (quote
       ("~/org/todo.org"
        "~/org/refile.org"
        "~/org/knowledge.org"
        "~/org/projects")))

;; (setq org-agenda-custom-commands
;;       (quote (("N" "Notes" tags "NOTE"
;;                ((org-agenda-overriding-header "Notes")
;;                 (org-tags-match-list-sublevels t))))))

(global-set-key (kbd "M-n") 'outline-next-visible-heading)
(global-set-key (kbd "M-p") 'outline-previous-visible-heading)

(setq org-use-fast-todo-selection t)

;; Org-capture
(global-set-key "\C-cc" 'org-capture)

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-completion-use-ido t)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-capture-templates
      (quote (
              ("t" "Todo" entry (file org-default-notes-file)
               "* TODO %?\n%U")
              ("j" "Journal" entry (file "~/org/journal.org")
               "* %U - %?")
              ("n" "Note" entry (file org-default-notes-file )
               "* NOTE %?\n%U")
              ("m" "Meeting" entry (file org-default-notes-file )
               "* MEETING %? :meeting: \n%U")
              ("p" "Phone" entry (file org-default-notes-file )
               "* PHONE %? :phone:\n%U"))))


;; org-mode.el ends here
