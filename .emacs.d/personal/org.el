;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'org)
(require 'org-agenda)

(setq org-log-done t)
(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)
(setq org-startup-folded t)

;; Org-agenda
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 7)
(setq org-deadline-warning-days 14)
(setq org-agenda-compact-blocks t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-window-setup 'current-window)

(setq org-todo-keywords '((sequence "TODO(t)" "NOTE(o)" "STARTED(n@)" "DEFERRED(e@)" "CANCELLED(c@)" "|" "DONE(d@)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NOTE" . "DeepSkyBlue4")
        ("STARTED" . "cyan3")
        ("CANCELLED" . "DarkRed")
        ("DEFERRED" . "disabledControlTextColor")
        ("DONE" . "green")))

(setq org-agenda-files
      (quote
       ("~/org/todo.org"
        "~/org/refile.org"
        "~/org/datalogi"
        "~/org/projects")))

(setq org-use-fast-todo-selection t)

(defun rca/org-archive-done-tasks ()
  "Archives all done items in the file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; Org-capture
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-capture-templates
      (quote (
              ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
               "* TODO %?\n%U")
              ("b" "Book" entry (file+headline "~/org/books.org" "Book list")
               "** [#C] %?")
              ("j" "Journal" entry (file "~/org/journal.org")
               "* %U - %?")
              ("q" "Quote" entry (file "~/org/quotes.org")
               "* \"%?\"")
              ("n" "Note" entry (file org-default-notes-file )
               "* NOTE %?\n%U"))))

;; Clocking - Taken from Purcell
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

;; Projetile functions (taken from glynnforrest/emacs.d)
(require 'projectile)
(setq helm-projectile-sources-list '(helm-source-projectile-buffers-list helm-source-projectile-files-list))

(defvar org-projects-dir (expand-file-name  "~/org/projects"))

(defun gf/create-org-path (path)
  "Create a name suitable for an org file from the last part of a file
path."
  (let ((last (car (last (split-string (if (equal (substring path -1) "/")
                                           (substring path 0 -1) path) "/")))))
    (concat org-projects-dir "/"
            (downcase
             (replace-regexp-in-string
              "\\." "-" (if (equal (substring last 0 1) ".")
                            (substring last 1) last)))
            ".org")))

(defun gf/project-org-file ()
  "Get the path of the org file for the current project."
  (gf/create-org-path (projectile-project-root)))

(defun gf/switch-to-project-org-file ()
  "Switch to the org file for the current project."
  (interactive)
  (find-file (gf/project-org-file)))

(defvar gf/previous-project-buffers (make-hash-table :test 'equal))

(defun gf/toggle-switch-to-project-org-file ()
  "Alternate between the current buffer and the org file for the
current project."
  (interactive)
  (if (and
       (string-equal "org-mode" (symbol-name major-mode))
       (s-contains-p "/notes/" (buffer-file-name)))
      (if (gethash (buffer-file-name) gf/previous-project-buffers)
          (switch-to-buffer (gethash (buffer-file-name) gf/previous-project-buffers))
        (error "Previous project buffer not found"))
    (let ((file (gf/project-org-file)))
      (puthash file (current-buffer) gf/previous-project-buffers)
      (find-file file)
      )))

;; Habits

(add-to-list 'org-modules 'org-habit t)
(provide 'org)
;;; org.el ends here
