;;; journal.el --- Simple timestamped journaling -*- lexical-binding: t; -*-

(defgroup rchrand-journal nil
  "Simple timestamped journaling."
  :group 'convenience)

(defcustom rchrand/journal-file-extension "org"
  "File extension to use for journal entries: \"org\" or \"md\"."
  :type '(choice (const :tag "Org" "org") (const :tag "Markdown" "md"))
  :group 'rchrand-journal)

(defcustom rchrand/journal-template-org
  (mapconcat #'identity
             '("#+TITLE: Journal %ts%"
               "#+DATE: %date%"
               "#+FILETAGS: :journal:"
               "#+OPTIONS: toc:nil"
               ""
               "* Quick Start"
               "- Spend 5–10 minutes; write first, don't edit."
               "- Use the prompts below; delete guidance as you go."
               ""
               "* Feeling"
               "Right now I feel ... because ..."
               ""
               "* Thinking"
               "What's looping in my head is ..."
               ""
               "* Next Tiny Step"
               "One tiny thing I can do is ..."
               ""
               "* Gratitude (optional)"
               "- "
               ""
               "* Worry parking lot (optional)"
               "- "
               "")
             "\n")
  "Template content for new Org journal entries. Supports tokens %ts% and %date%."
  :type 'string
  :group 'rchrand-journal)

(defcustom rchrand/journal-template-md
  (mapconcat #'identity
             '("# Journal %ts%"
               "_Date: %date%_"
               ""
               "## Quick Start"
               "- Spend 5–10 minutes; write first, don't edit."
               "- Use the prompts below; delete guidance as you go."
               ""
               "## Feeling"
               "Right now I feel ... because ..."
               ""
               "## Thinking"
               "What's looping in my head is ..."
               ""
               "## Next Tiny Step"
               "One tiny thing I can do is ..."
               ""
               "## Gratitude (optional)"
               "- "
               ""
               "## Worry parking lot (optional)"
               "- "
               "")
             "\n")
  "Template content for new Markdown journal entries. Supports tokens %ts% and %date%."
  :type 'string
  :group 'rchrand-journal)

(defcustom rchrand/journal-subdir "notes/journal"
  "Subdirectory inside Dropbox where journal files are stored."
  :type 'string
  :group 'rchrand-journal)

(defun rchrand/journal--dropbox-root ()
  "Return the Dropbox root directory, handling macOS variants."
  (let ((p1 (expand-file-name "~/Dropbox"))
        (p2 (expand-file-name "~/Library/CloudStorage/Dropbox")))
    (cond ((file-directory-p p1) p1)
          ((file-directory-p p2) p2)
          (t p1))))

(defun rchrand/journal-directory ()
  "Absolute path to the journal directory."
  (expand-file-name rchrand/journal-subdir (rchrand/journal--dropbox-root)))

(defun rchrand/journal-new-entry ()
  "Create a new timestamped journal file and open it."
  (interactive)
  (let* ((dir (rchrand/journal-directory))
         (ts (format-time-string "%Y-%m-%d-%H-%M-%S"))
         (date (format-time-string "%Y-%m-%d %H:%M:%S"))
         (ext rchrand/journal-file-extension)
         (fn (expand-file-name (concat ts "." ext) dir))
         (new (not (file-exists-p fn)))
         (tmpl (if (string= ext "org")
                   rchrand/journal-template-org
                 rchrand/journal-template-md)))
    (make-directory dir t)
    (find-file fn)
    (when new
      (let* ((content (replace-regexp-in-string
                       "%date%" date
                       (replace-regexp-in-string "%ts%" ts tmpl t t)
                       t t)))
        (insert content)
        (goto-char (point-min))
        (if (search-forward "Right now I feel" nil t)
            (end-of-line)
          (goto-char (point-max)))))))

(when (file-directory-p (rchrand/journal--dropbox-root))
  (global-set-key (kbd "C-c j") #'rchrand/journal-new-entry))

(provide 'journal)
;;; journal.el ends here
