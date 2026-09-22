;;; org-work.el --- Daily work notes and dashboard -*- lexical-binding: t; -*-

(require 'button)
(require 'org)
(require 'org-capture)
(require 'seq)
(require 'subr-x)

(defgroup rchrand-org-work nil
  "Small daily work-note workflow."
  :group 'org)

(defcustom rchrand/org-work-directory
  (expand-file-name "~/projects/notes/org/")
  "Root directory for raw Org work notes."
  :type 'directory
  :group 'rchrand-org-work)

(defcustom rchrand/org-obsidian-work-directory
  (expand-file-name "~/projects/notes/Work/")
  "Obsidian directory for AI-compiled work notes."
  :type 'directory
  :group 'rchrand-org-work)

(defcustom rchrand/org-dashboard-recent-count 10
  "Number of daily notes shown on the dashboard."
  :type 'integer
  :group 'rchrand-org-work)

(defcustom rchrand/org-ai-brief-script
  (expand-file-name "bin/org-daily-brief" user-emacs-directory)
  "Program that compiles recent daily notes into an AI briefing."
  :type 'file
  :group 'rchrand-org-work)

(defconst rchrand/org-dashboard-buffer-name "*Work Dashboard*")

(defvar rchrand/org-ai-process nil
  "Current AI briefing process, if one is running.")

(defun rchrand/org-daily-directory ()
  "Return the daily-note directory."
  (expand-file-name "daily/" rchrand/org-work-directory))

(defun rchrand/org-briefing-file ()
  "Return the generated briefing path."
  (expand-file-name "Current Work Briefing.md"
                    rchrand/org-obsidian-work-directory))

(defun rchrand/org-knowledge-candidates-file ()
  "Return the generated knowledge-candidate path."
  (expand-file-name "Knowledge Candidates.md"
                    rchrand/org-obsidian-work-directory))

(defun rchrand/org-weekly-directory ()
  "Return the generated weekly-synthesis directory."
  (expand-file-name "Weekly/" rchrand/org-obsidian-work-directory))

(defun rchrand/org-latest-weekly-file ()
  "Return the newest weekly synthesis, if one exists."
  (let ((directory (rchrand/org-weekly-directory)))
    (when (file-directory-p directory)
      (car (sort (directory-files directory t
                                  "^[0-9]\\{4\\}-W[0-9]\\{2\\}\\.md$"
                                  t)
                 #'string-greaterp)))))

(defun rchrand/org-daily-file (&optional time)
  "Return the daily-note file for TIME, or today."
  (expand-file-name (format-time-string "%Y-%m-%d.org" time)
                    (rchrand/org-daily-directory)))

(defun rchrand/org-daily-files ()
  "Return daily-note paths, newest first."
  (let ((directory (rchrand/org-daily-directory)))
    (if (file-directory-p directory)
        (sort (directory-files directory t "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$")
              #'string-greaterp)
      nil)))

(defun rchrand/org-daily-previous-file (&optional time)
  "Return the newest daily note before TIME, or before today."
  (let ((target (format-time-string "%Y-%m-%d" (or time (current-time)))))
    (seq-find (lambda (file)
                (string< (file-name-base file) target))
              (rchrand/org-daily-files))))

(defun rchrand/org-daily--section-bounds (heading)
  "Return content bounds for top-level HEADING in the current buffer."
  (goto-char (point-min))
  (when (re-search-forward (format "^\\* %s[ \\t]*$" (regexp-quote heading))
                           nil t)
    (let ((start (line-beginning-position 2)))
      (cons start
            (save-excursion
              (goto-char start)
              (if (re-search-forward "^\\* " nil t)
                  (match-beginning 0)
                (point-max)))))))

(defun rchrand/org-daily--task-tree-end (limit indentation)
  "Return the end of the current task tree before LIMIT.
INDENTATION is the root task's indentation level."
  (save-excursion
    (forward-line 1)
    (let (end)
      (while (and (< (point) limit) (not end))
        (when (and (looking-at "^[ \\t]*[-+] \\[[ xX-]\\] .+")
                   (<= (current-indentation) indentation))
          (setq end (line-beginning-position)))
        (unless end
          (forward-line 1)))
      (or end limit))))

(defun rchrand/org-daily--unfinished-trees-in-section (file heading)
  "Return unfinished top-level task trees below HEADING in FILE.
An unfinished or partial parent carries its complete subtree, including
completed descendants.  A completed parent suppresses its subtree."
  (with-temp-buffer
    (insert-file-contents file)
    (when-let* ((bounds (rchrand/org-daily--section-bounds heading)))
      (goto-char (car bounds))
      (let (trees)
        (while (re-search-forward
                "^[ \\t]*[-+] \\(\\[[ xX-]\\]\\) .+$" (cdr bounds) t)
          (goto-char (match-beginning 0))
          (let* ((start (point))
                 (state (match-string 1))
                 (end (rchrand/org-daily--task-tree-end
                       (cdr bounds) (current-indentation))))
            (unless (member state '("[X]" "[x]"))
              (push (string-trim-right
                     (buffer-substring-no-properties start end))
                    trees))
            (goto-char end)))
        (nreverse trees)))))

(defun rchrand/org-daily-unfinished-items (file)
  "Return unfinished work carried by FILE's work sections."
  (delete-dups
   (append (rchrand/org-daily--unfinished-trees-in-section file "Unfinished")
           (rchrand/org-daily--unfinished-trees-in-section file "Today"))))

(defun rchrand/org-daily-template (&optional time unfinished-items)
  "Return a daily-note template for TIME with UNFINISHED-ITEMS."
  (let ((time (or time (current-time))))
    (format (concat "#+title: %s\n"
                    "#+date: [%s]\n"
                    "#+filetags: :daily:\n"
                    "#+startup: showall\n\n"
                    "%s"
                    "* Today\n\n"
                    "* Notes\n\n"
                    "* Wrap-up\n")
            (format-time-string "%A, %d %B %Y" time)
            (format-time-string "%Y-%m-%d %a" time)
            (if unfinished-items
                (format "* Unfinished\n%s\n\n"
                        (string-join unfinished-items "\n"))
              ""))))

(defun rchrand/org-daily-ensure (&optional time)
  "Create and return the daily note for TIME, or today."
  (let ((file (rchrand/org-daily-file time)))
    (make-directory (file-name-directory file) t)
    (unless (file-exists-p file)
      (let* ((previous (rchrand/org-daily-previous-file time))
             (unfinished (when previous
                           (rchrand/org-daily-unfinished-items previous))))
        (with-temp-file file
          (insert (rchrand/org-daily-template time unfinished)))))
    file))

(defun rchrand/org-daily-open (&optional time)
  "Open the daily note for TIME, or today."
  (interactive)
  (find-file (rchrand/org-daily-ensure time))
  (org-show-all)
  (goto-char (point-min))
  (if (re-search-forward "^- \\[ \\] ?$" nil t)
      (progn
        (goto-char (line-end-position))
        (unless (looking-at-p "\n\n")
          (save-excursion (insert "\n"))))
    (rchrand/org-daily--capture-target "Today")
    (insert "- [ ] ")
    (save-excursion (insert "\n"))))

(defun rchrand/org-daily-open-date (date)
  "Prompt for and open the daily note for DATE."
  (interactive (list (org-read-date nil nil nil "Daily note date: ")))
  (rchrand/org-daily-open (org-time-string-to-time date)))

(defun rchrand/org-daily--capture-target (heading)
  "Visit today's note and move to the end of HEADING."
  (find-file (rchrand/org-daily-ensure))
  (widen)
  (goto-char (point-min))
  (unless (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote heading))
                             nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* %s\n" heading)))
  (org-back-to-heading t)
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  (when (looking-at-p "^\\*")
    (insert "\n")
    (backward-char)))

(defun rchrand/org-capture-today-focus ()
  "Target today's Focus section for capture."
  (rchrand/org-daily--capture-target "Today"))

(defun rchrand/org-capture-today-note ()
  "Target today's Notes section for capture."
  (rchrand/org-daily--capture-target "Notes"))

(defun rchrand/org-capture-focus ()
  "Capture a checkbox under today's focus list."
  (interactive)
  (org-capture nil "f"))

(defun rchrand/org-capture-note ()
  "Capture a timestamped note under today's Notes section."
  (interactive)
  (org-capture nil "o"))

(defun rchrand/org-daily-checkbox-counts (file)
  "Return (DONE . TOTAL) checkbox counts for FILE's Today section."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t)
          (done 0)
          (total 0))
      (when-let* ((bounds (rchrand/org-daily--section-bounds "Today")))
        (goto-char (car bounds))
        (while (re-search-forward
                "^[ \\t]*[-+] \\[\\([ xX-]\\)\\] .+" (cdr bounds) t)
          (setq total (1+ total))
          (when (member (match-string 1) '("x" "X"))
            (setq done (1+ done)))))
      (cons done total))))

(defun rchrand/org-dashboard--insert-action (label action help)
  "Insert a dashboard button with LABEL, ACTION, and HELP."
  (insert-text-button label
                      'follow-link t
                      'help-echo help
                      'action (lambda (_button) (funcall action))))

(defun rchrand/org-dashboard--insert-briefing ()
  "Insert the current AI briefing preview."
  (let ((file (rchrand/org-briefing-file))
        (candidates (rchrand/org-knowledge-candidates-file))
        (weekly (rchrand/org-latest-weekly-file)))
    (insert "AI briefing\n")
    (insert (make-string 12 ?─) "\n")
    (if (not (file-exists-p file))
        (insert "Not generated yet. Press a to compile recent notes with Luna.\n")
      (rchrand/org-dashboard--insert-action
       "Open full briefing"
       (lambda ()
         (rchrand/org-dashboard--call-in-main-window #'find-file file))
       file)
      (when (file-exists-p candidates)
        (insert "   ")
        (rchrand/org-dashboard--insert-action
         "Knowledge candidates"
         (lambda ()
           (rchrand/org-dashboard--call-in-main-window #'find-file candidates))
         candidates))
      (when weekly
        (insert "   ")
        (rchrand/org-dashboard--insert-action
         (format "Latest weekly: %s" (file-name-base weekly))
         (lambda ()
           (rchrand/org-dashboard--call-in-main-window #'find-file weekly))
         weekly))
      (insert "\n\n")
      (let* ((lines (with-temp-buffer
                      (insert-file-contents file)
                      (split-string (buffer-string) "\n")))
             (body (seq-filter
                    (lambda (line)
                      (and (not (string= "---" line))
                           (not (string-match-p
                                 "\\`\\(type\\|generated\\|sources\\):" line))
                           (not (string-match-p "\\`  - [0-9]" line))
                           (not (string-empty-p line))))
                    lines)))
        (dolist (line (seq-take body 14))
          (insert line "\n"))))
    (insert "\n")))

(defun rchrand/org-dashboard--insert-recent ()
  "Insert recent daily-note links."
  (insert "Recent daily notes\n")
  (insert (make-string 18 ?─) "\n")
  (let ((files (seq-take (rchrand/org-daily-files)
                         rchrand/org-dashboard-recent-count)))
    (if (null files)
        (insert "No daily notes yet.\n")
      (dolist (file files)
        (let* ((name (file-name-base file))
               (counts (rchrand/org-daily-checkbox-counts file))
               (label (format "%s  %d/%d" name (car counts) (cdr counts))))
          (insert "  ")
          (rchrand/org-dashboard--insert-action
           label
           (lambda ()
             (rchrand/org-dashboard--call-in-main-window #'find-file file))
           file)
          (insert "\n"))))
    (insert "\n")))

(defun rchrand/org-dashboard-refresh ()
  "Redraw the work dashboard."
  (interactive)
  (let ((inhibit-read-only t)
        (today-file (rchrand/org-daily-file)))
    (erase-buffer)
    (insert (propertize "WORK / TODAY\n" 'face '(:height 1.5 :weight bold)))
    (insert (format-time-string "%A, %d %B %Y\n\n"))
    (rchrand/org-dashboard--insert-action
     "Open today" #'rchrand/org-dashboard-open-today
     "Create or open today's note")
    (insert "   ")
    (rchrand/org-dashboard--insert-action
     "Add focus" #'rchrand/org-dashboard-capture-focus
     "Add a checkbox to today's focus")
    (insert "   ")
    (rchrand/org-dashboard--insert-action
     "Add note" #'rchrand/org-dashboard-capture-note "Add a timestamped note")
    (insert "   ")
    (rchrand/org-dashboard--insert-action
     "AI briefing" #'rchrand/org-ai-brief "Compile recent notes with Luna")
    (insert "   ")
    (rchrand/org-dashboard--insert-action
     "Weekly synthesis" #'rchrand/org-ai-weekly
     "Compile the previous completed week with Luna")
    (insert "\n\n")
    (if (file-exists-p today-file)
        (let ((counts (rchrand/org-daily-checkbox-counts today-file)))
          (insert (format "Today's focus: %d/%d complete\n\n"
                          (car counts) (cdr counts))))
      (insert "Today's note has not been created yet.\n\n"))
    (rchrand/org-dashboard--insert-briefing)
    (rchrand/org-dashboard--insert-recent)
    (insert "Keys: d today · D date · f focus · o note · n/p move · a AI · w weekly · b briefing · k candidates · g refresh\n")
    (goto-char (point-min))))

(defvar rchrand/org-dashboard-mode-map
  (make-sparse-keymap)
  "Keymap for `rchrand/org-dashboard-mode'.")

;; Keep these outside `defvar' so reloading the config updates existing maps.
(set-keymap-parent rchrand/org-dashboard-mode-map special-mode-map)
(define-key rchrand/org-dashboard-mode-map (kbd "t") #'rchrand/org-dashboard-open-today)
(define-key rchrand/org-dashboard-mode-map (kbd "d") #'rchrand/org-dashboard-open-today)
(define-key rchrand/org-dashboard-mode-map (kbd "D") #'rchrand/org-dashboard-open-date)
(define-key rchrand/org-dashboard-mode-map (kbd "f") #'rchrand/org-dashboard-capture-focus)
(define-key rchrand/org-dashboard-mode-map (kbd "o") #'rchrand/org-dashboard-capture-note)
(define-key rchrand/org-dashboard-mode-map (kbd "n") #'next-line)
(define-key rchrand/org-dashboard-mode-map (kbd "p") #'previous-line)
(define-key rchrand/org-dashboard-mode-map (kbd "a") #'rchrand/org-ai-brief)
(define-key rchrand/org-dashboard-mode-map (kbd "w") #'rchrand/org-ai-weekly)
(define-key rchrand/org-dashboard-mode-map (kbd "b") #'rchrand/org-dashboard-open-briefing)
(define-key rchrand/org-dashboard-mode-map (kbd "k") #'rchrand/org-dashboard-open-knowledge-candidates)
(define-key rchrand/org-dashboard-mode-map (kbd "g") #'rchrand/org-dashboard-refresh)

(define-derived-mode rchrand/org-dashboard-mode special-mode "Work-Dashboard"
  "Major mode for the daily work dashboard."
  (setq-local revert-buffer-function
              (lambda (&rest _args) (rchrand/org-dashboard-refresh))))

(defun rchrand/org-dashboard-buffer ()
  "Return the refreshed dashboard buffer without displaying it."
  (let ((buffer (get-buffer-create rchrand/org-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'rchrand/org-dashboard-mode)
        (rchrand/org-dashboard-mode))
      (rchrand/org-dashboard-refresh))
    buffer))

(defun rchrand/org-dashboard ()
  "Toggle the work dashboard as a bottom popup."
  (interactive)
  (let ((buffer (rchrand/org-dashboard-buffer)))
    (cond
     ((rchrand/org-dashboard--popup-window buffer)
      (quit-window nil (rchrand/org-dashboard--popup-window buffer)))
     ((get-buffer-window buffer)
      (select-window (get-buffer-window buffer)))
     ((fboundp 'popper-select-popup-at-bottom)
      (popper-select-popup-at-bottom buffer))
     (t (pop-to-buffer buffer)))
    buffer))

(defun rchrand/org-open-briefing ()
  "Open the generated AI briefing."
  (interactive)
  (let ((file (rchrand/org-briefing-file)))
    (if (file-exists-p file)
        (find-file file)
      (user-error "No briefing yet; press a on the dashboard to generate one"))))

(defun rchrand/org-open-knowledge-candidates ()
  "Open the generated knowledge candidates."
  (interactive)
  (let ((file (rchrand/org-knowledge-candidates-file)))
    (if (file-exists-p file)
        (find-file file)
      (user-error "No knowledge candidates yet; generate a briefing first"))))

(defun rchrand/org-ai--run (&optional weekly)
  "Run the Luna compiler, generating a WEEKLY synthesis when non-nil."
  (when (process-live-p rchrand/org-ai-process)
    (user-error "An AI compilation is already running"))
  (unless (rchrand/org-daily-files)
    (user-error "Create a daily note before running AI compilation"))
  (unless (file-executable-p rchrand/org-ai-brief-script)
    (user-error "Briefing script is not executable: %s" rchrand/org-ai-brief-script))
  (let* ((label (if weekly "weekly synthesis" "briefing"))
         (buffer (get-buffer-create "*Org AI Brief*"))
         (command (append (list rchrand/org-ai-brief-script)
                          (when weekly '("--weekly")))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Generating %s with GPT-5.6 Luna…\n" label))))
    (setq rchrand/org-ai-process
          (make-process
           :name "org-ai-brief"
           :buffer buffer
           :command command
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (if (= (process-exit-status process) 0)
                   (progn
                     (message "AI %s generated" label)
                     (when-let* ((dashboard
                                  (get-buffer rchrand/org-dashboard-buffer-name)))
                       (with-current-buffer dashboard
                         (rchrand/org-dashboard-refresh))))
                 (display-buffer (process-buffer process))
                 (message "AI %s failed; see %s"
                          label (buffer-name (process-buffer process))))))))
    (message "Generating AI %s…" label)))

(defun rchrand/org-ai-brief ()
  "Compile recent daily notes into Obsidian with Luna."
  (interactive)
  (rchrand/org-ai--run))

(defun rchrand/org-ai-weekly ()
  "Compile the previous completed week into an Obsidian snapshot."
  (interactive)
  (rchrand/org-ai--run t))

(setq org-directory rchrand/org-work-directory
      org-default-notes-file (expand-file-name "inbox.org" rchrand/org-work-directory)
      org-capture-templates
      '(("f" "Today's focus" plain (function rchrand/org-capture-today-focus)
         "- [ ] %?\n" :empty-lines 0)
        ("o" "Today's note" plain (function rchrand/org-capture-today-note)
         "- %U %?\n" :empty-lines 0)))

(setq initial-buffer-choice #'rchrand/org-dashboard-buffer)

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c d") #'rchrand/org-dashboard)
(global-set-key (kbd "C-c j") #'rchrand/org-daily-open)

(provide 'org-work)
;;; org-work.el ends here
