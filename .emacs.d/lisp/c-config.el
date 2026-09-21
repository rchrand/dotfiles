;;; c-config.el --- Plain C workflow for small projects -*- lexical-binding: t; -*-

(require 'project)
(require 'subr-x)

(defgroup rchrand-c nil
  "Simple C defaults for editing, building, and running small projects."
  :group 'languages)

(defcustom rchrand/c-build-directory "build"
  "Directory used for ad-hoc compiled binaries."
  :type 'string
  :group 'rchrand-c)

(defcustom rchrand/c-compiler "cc"
  "Compiler used for simple single-file C builds."
  :type 'string
  :group 'rchrand-c)

(defvar-local rchrand/c-run-command nil
  "Run command associated with the current C buffer.")

(defconst rchrand/c-project-root-files
  '(".git" "GNUmakefile" "Makefile" "makefile" "justfile" "Justfile" "CMakeLists.txt")
  "Files that identify a useful C project root.")

(with-eval-after-load 'treesit-auto
  ;; Keep C/C++ on the built-in major modes. No grammar prompts; less magic.
  (setq treesit-auto-recipe-list
        (seq-remove
         (lambda (recipe)
           (memq (treesit-auto-recipe-remap recipe) '(c-mode c++-mode)))
         treesit-auto-recipe-list)))

(defun rchrand/c--project-root ()
  "Return current project root or `default-directory'."
  (let* ((start-dir (or (when-let* ((file (buffer-file-name)))
                           (file-name-directory file))
                        default-directory))
         (root-from-markers
          (seq-some (lambda (marker)
                      (locate-dominating-file start-dir marker))
                    rchrand/c-project-root-files)))
    (or root-from-markers
        (and (fboundp 'projectile-project-root)
             (ignore-errors (projectile-project-root)))
        (when-let* ((project (project-current nil default-directory)))
          (car (project-roots project)))
        (when (and (string-match-p "/\\(?:src\\|source\\)/\\'" start-dir)
                   (file-directory-p (directory-file-name start-dir)))
          (file-name-directory (directory-file-name start-dir)))
        start-dir)))

(defun rchrand/c--project-file (&rest parts)
  "Expand PARTS from the current project root."
  (expand-file-name (mapconcat #'identity parts "/")
                    (rchrand/c--project-root)))

(defun rchrand/c--first-existing-file (&rest candidates)
  "Return first existing file from CANDIDATES relative to project root."
  (seq-find #'file-exists-p
            (mapcar (lambda (candidate)
                      (rchrand/c--project-file candidate))
                    candidates)))

(defun rchrand/c--makefile ()
  "Return current project Makefile path, if any."
  (rchrand/c--first-existing-file "GNUmakefile" "Makefile" "makefile"))

(defun rchrand/c--justfile ()
  "Return current project justfile path, if any."
  (rchrand/c--first-existing-file "justfile" "Justfile"))

(defun rchrand/c--file-has-target-p (file target)
  "Return non-nil when FILE defines TARGET."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (re-search-forward (format "^%s\\s-*:" (regexp-quote target)) nil t))))

(defun rchrand/c--entry-file ()
  "Return likely entry C file for the current project."
  (or (rchrand/c--first-existing-file "src/main.c" "main.c" "source/main.c")
      (when-let* ((file (buffer-file-name)))
        (when (string-match-p "\\.c\\'" file)
          file))))

(defun rchrand/c--output-name ()
  "Return output binary name for ad-hoc builds."
  (let* ((entry-file (rchrand/c--entry-file))
         (entry-base (and entry-file (file-name-base entry-file)))
         (project-name (file-name-nondirectory
                        (directory-file-name (rchrand/c--project-root)))))
    (if (and entry-base (string= entry-base "main"))
        project-name
      (or entry-base project-name "app"))))

(defun rchrand/c--output-path ()
  "Return output binary path for ad-hoc builds."
  (rchrand/c--project-file rchrand/c-build-directory (rchrand/c--output-name)))

(defun rchrand/c--pkg-config-has-raylib-p ()
  "Return non-nil when pkg-config can resolve raylib."
  (and (executable-find "pkg-config")
       (eq 0 (call-process "pkg-config" nil nil nil "--exists" "raylib"))))

(defun rchrand/c--pkg-config-cflags (&rest packages)
  "Return `pkg-config --cflags' tokens for PACKAGES."
  (when (and packages (executable-find "pkg-config"))
    (with-temp-buffer
      (when (eq 0 (apply #'call-process "pkg-config" nil t nil "--cflags" packages))
        (split-string-and-unquote (string-trim (buffer-string)))))))

(defun rchrand/c--pkg-config-include-paths (&rest packages)
  "Return include paths from `pkg-config --cflags' for PACKAGES."
  (let ((tokens (apply #'rchrand/c--pkg-config-cflags packages))
        includes)
    (while tokens
      (let ((token (pop tokens)))
        (cond
         ((string-prefix-p "-I" token)
          (push (substring token 2) includes))
         ((string= token "-I")
          (when tokens
            (push (pop tokens) includes))))))
    (nreverse includes)))

(defun rchrand/c--pkg-config-defines (&rest packages)
  "Return preprocessor definitions from `pkg-config --cflags' for PACKAGES."
  (let ((tokens (apply #'rchrand/c--pkg-config-cflags packages))
        defines)
    (while tokens
      (let ((token (pop tokens)))
        (cond
         ((string-prefix-p "-D" token)
          (push token defines))
         ((string= token "-D")
          (when tokens
            (push (concat "-D" (pop tokens)) defines))))))
    (nreverse defines)))

(defun rchrand/c--raylib-include-dir ()
  "Return the primary raylib include directory."
  (car (rchrand/c--pkg-config-include-paths "raylib")))

(defun rchrand/c--raylib-header-path ()
  "Return the `raylib.h' path resolved through pkg-config."
  (when-let* ((include-dir (rchrand/c--raylib-include-dir)))
    (expand-file-name "raylib.h" include-dir)))

(defun rchrand/c--project-tags-file ()
  "Return TAGS file path for the current project."
  (rchrand/c--project-file "TAGS"))

(defun rchrand/c--project-source-files ()
  "Return project C/C header files for tags generation."
  (seq-filter #'file-regular-p
              (append
               (directory-files-recursively (rchrand/c--project-root) "\\.[ch]\\'")
               (directory-files-recursively (rchrand/c--project-root) "\\.hpp?\\'"))))

(defun rchrand/c--raylib-header-files ()
  "Return raylib headers installed through pkg-config."
  (when-let* ((include-dir (rchrand/c--raylib-include-dir))
              ((file-directory-p include-dir)))
    (directory-files include-dir t "\\.h\\'")))

(defun rchrand/c-load-tags ()
  "Load project TAGS file if it exists."
  (interactive)
  (let ((tags-file (rchrand/c--project-tags-file)))
    (when (file-exists-p tags-file)
      (visit-tags-table tags-file t)
      tags-file)))

(defun rchrand/c-generate-tags ()
  "Generate and load a TAGS file for the current C project and raylib headers."
  (interactive)
  (let* ((default-directory (rchrand/c--project-root))
         (tags-file (rchrand/c--project-tags-file))
         (source-files (delete-dups
                        (append (rchrand/c--project-source-files)
                                (rchrand/c--raylib-header-files)))))
    (unless source-files
      (user-error "No C/header files found for TAGS"))
    (unless (executable-find "etags")
      (user-error "etags not found"))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (with-current-buffer (get-buffer-create "*c-tags*")
      (erase-buffer)
      (let ((status (apply #'process-file "etags" nil t nil "-o" tags-file source-files)))
        (unless (eq status 0)
          (error "etags failed; see *c-tags* buffer"))))
    (visit-tags-table tags-file t)
    (message "Loaded TAGS: %s" tags-file)))

(defun rchrand/c--ad-hoc-build-command ()
  "Return a direct raylib build command for a single-file project."
  (when-let* ((entry-file (rchrand/c--entry-file))
              (root (rchrand/c--project-root))
              (_ (rchrand/c--pkg-config-has-raylib-p)))
    (let* ((entry-rel (file-relative-name entry-file root))
           (output-path (rchrand/c--output-path))
           (output-rel (file-relative-name output-path root))
           (output-dir-rel (file-relative-name (file-name-directory output-path) root)))
      (string-join
       (list
        (format "mkdir -p %s" (shell-quote-argument output-dir-rel))
        (format "%s -std=c11 -Wall -Wextra -pedantic %s -o %s $(pkg-config --cflags --libs raylib)"
                (shell-quote-argument rchrand/c-compiler)
                (shell-quote-argument entry-rel)
                (shell-quote-argument output-rel)))
       " && "))))

(defun rchrand/c-detect-build-command ()
  "Return default build command for the current C project."
  (cond
   ((rchrand/c--makefile) "make")
   ((and (rchrand/c--justfile) (executable-find "just"))
    (if (rchrand/c--file-has-target-p (rchrand/c--justfile) "build")
        "just build"
      "just"))
   ((and (file-exists-p (rchrand/c--project-file "CMakeLists.txt"))
         (file-directory-p (rchrand/c--project-file "build"))
         (executable-find "cmake"))
    "cmake --build build")
   ((rchrand/c--ad-hoc-build-command))
   (t "make")))

(defun rchrand/c-detect-run-command ()
  "Return default run command for the current C project."
  (let ((makefile (rchrand/c--makefile))
        (justfile (rchrand/c--justfile))
        (output-rel (file-relative-name (rchrand/c--output-path)
                                        (rchrand/c--project-root))))
    (cond
     ((rchrand/c--file-has-target-p makefile "run") "make run")
     ((and justfile
           (executable-find "just")
           (rchrand/c--file-has-target-p justfile "run"))
      "just run")
     ((rchrand/c--ad-hoc-build-command)
      (if (string-prefix-p "./" output-rel)
          output-rel
        (concat "./" output-rel)))
     (t nil))))

(defun rchrand/c-refresh-build-commands ()
  "Refresh local build and run commands for the current C buffer."
  (interactive)
  (setq-local compile-command (rchrand/c-detect-build-command)
              rchrand/c-run-command (rchrand/c-detect-run-command)))

(defun rchrand/c-build (&optional prompt)
  "Build current C project.
With PROMPT, read the command in the minibuffer."
  (interactive "P")
  (let ((default-directory (rchrand/c--project-root)))
    (unless prompt
      (rchrand/c-refresh-build-commands))
    (call-interactively #'compile)))

(defun rchrand/c-run (&optional prompt)
  "Run current C project.
With PROMPT, read the run command in the minibuffer."
  (interactive "P")
  (let ((default-directory (rchrand/c--project-root)))
    (unless prompt
      (rchrand/c-refresh-build-commands))
    (if-let* ((command (if prompt
                           (read-shell-command "Run command: " rchrand/c-run-command)
                         rchrand/c-run-command)))
        (compile command)
      (user-error "No run command detected; set one with `M-x compile' or use C-u C-c C-r"))))

(defun rchrand/c-build-and-run ()
  "Build, then run current C project."
  (interactive)
  (let ((default-directory (rchrand/c--project-root)))
    (rchrand/c-refresh-build-commands)
    (if-let* ((run-command rchrand/c-run-command))
        (compile (format "%s && %s" compile-command run-command))
      (user-error "No run command detected for this project"))))

(defun rchrand/c-verify-tools ()
  "Report availability of common C and raylib tools."
  (interactive)
  (let ((cc (executable-find rchrand/c-compiler))
        (make (executable-find "make"))
        (just (executable-find "just"))
        (pkg-config (executable-find "pkg-config"))
        (raylib (rchrand/c--pkg-config-has-raylib-p)))
    (message (mapconcat #'identity
                        (list (format "%s: %s" rchrand/c-compiler (if cc "OK" "MISSING"))
                              (format "make: %s" (if make "OK" "MISSING"))
                              (format "just: %s" (if just "OK" "MISSING"))
                              (format "pkg-config: %s" (if pkg-config "OK" "MISSING"))
                              (format "raylib pkg-config: %s" (if raylib "OK" "MISSING")))
                        ", "))))

(defun rchrand/open-raylib-header ()
  "Open `raylib.h' from the installed raylib package."
  (interactive)
  (if-let* ((header (rchrand/c--raylib-header-path))
            ((file-exists-p header)))
      (find-file header)
    (user-error "Could not resolve raylib.h via pkg-config")))

(defun rchrand/c-setup ()
  "Set up plain C editing defaults."
  (setq-local tab-width 4
              indent-tabs-mode nil
              c-basic-offset 4)
  (when (derived-mode-p 'c-mode)
    (c-set-style "bsd"))
  (when (boundp 'c-ts-mode-indent-offset)
    (setq-local c-ts-mode-indent-offset 4))
  (when (boundp 'flycheck-gcc-language-standard)
    (setq-local flycheck-gcc-language-standard "c11"
                flycheck-clang-language-standard "c11"))
  (when (boundp 'flycheck-clang-include-path)
    (let ((raylib-includes (rchrand/c--pkg-config-include-paths "raylib"))
          (raylib-defines (rchrand/c--pkg-config-defines "raylib")))
      (setq-local flycheck-clang-include-path raylib-includes
                  flycheck-gcc-include-path raylib-includes
                  flycheck-clang-args raylib-defines
                  flycheck-gcc-args raylib-defines)))
  (rchrand/c-load-tags)
  (rchrand/c-refresh-build-commands)
  (local-set-key (kbd "C-c b") #'rchrand/c-build)
  (local-set-key (kbd "C-c C-b") #'rchrand/c-build)
  (local-set-key (kbd "C-c r") #'rchrand/c-run)
  (local-set-key (kbd "C-c C-r") #'rchrand/c-run)
  (local-set-key (kbd "C-c C-t") #'rchrand/c-generate-tags)
  (local-set-key (kbd "<f5>") #'rchrand/c-build)
  (local-set-key (kbd "S-<f5>") #'rchrand/c-build-and-run))

(add-hook 'c-mode-hook #'rchrand/c-setup)
(with-eval-after-load 'c-ts-mode
  (add-hook 'c-ts-mode-hook #'rchrand/c-setup))
(global-set-key (kbd "C-c h r") #'rchrand/open-raylib-header)

(provide 'c-config)
;;; c-config.el ends here
