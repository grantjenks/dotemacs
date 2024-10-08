;; # Emacs Config File: .emacs
;;
;; Copyright 2015-2022 Grant Jenks
;;
;; ## Setup
;;
;;     $ cat ~/.emacs
;;     (load-file "~/repos/dotemacs/.emacs")
;;
;; ## Reminders
;;
;; /ssh:magnesium:/srv/www for remote access
;; /ssh:magnesium|sudo:magnesium:/etc for sudo access
;; C-SPC C-SPC set mark and disable region
;; C-u C-SPC jump to the mark
;; M-a / M-e forward / back sentence
;; C-M-a C-M-e begging/end of defun
;; C-x r (m|b|l) registers/bookmarks
;; C-x r s [name] save region to register
;; C-x r i [name] insert region from register
;;
;; ### Upgrade Packages
;;
;; 1. (package-refresh-contents)
;; 2. (package-list-packages)
;; 3. U  ; Select packages for upgrades.
;; 4. x  ; Upgrade packages.
;;
;; ### Elisp Commands
;;
;; M-: eval-expression
;; C-x C-e eval-last-sexp
;; [C-u] C-M-x [debug] eval-defun

;; (setq dotemacs-directory (file-name-directory load-file-name))
;; (add-to-list 'load-path dotemacs-directory)

;; (setq themes-directory (concat dotemacs-directory "themes"))
;; (add-to-list 'custom-theme-load-path themes-directory)
;; (cond ((display-graphic-p) (load-theme 'solarized t))
;;       (t (load-theme 'whiteboard)))

(mapc
 (lambda (path)
   (let ((bin-dir (expand-file-name path)))
     (when (file-directory-p bin-dir)
       (setenv "PATH" (concat (getenv "PATH") ":" bin-dir))
       (setq exec-path (append exec-path (list bin-dir))))))
 (list
  "/opt/local/bin"
  "/opt/local/sbin"
  "~/bin"
  "/usr/local/bin"
  "~/.local/bin"
  ))

(setq frame-title-format "%b")
(setq inhibit-startup-screen t)
(setq mac-command-modifier 'control)
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(setq-default sentence-end-double-space nil)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(setq create-lockfiles nil)

(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-so-long-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'rb 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'bw 'balance-windows)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'tail 'auto-revert-tail-mode)

(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-M-g") 'find-file-at-point)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-x w") 'delete-trailing-whitespace)

;; Configure backups.
(require 'tramp)
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)
(let ((backup-dir "~/.emacs-backups")
      (auto-saves-dir "~/.emacs-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        backup-directory-alist `(("." . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir)
        tramp-backup-directory-alist `((".*" . ,backup-dir)))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Hide window bars.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Use utf-8 everywhere.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Smooth scrolling.
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(require 'package)
(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Install selected packages.
(setq package-selected-packages
      '(
        ace-window
        bazel
        company
        concurrent
        csv-mode
        cython-mode
        dired-narrow
        dired-subtree
        ; docker
        dumb-jump
        ein
        emacsql
        emacsql-sqlite-builtin
        emojify
        exec-path-from-shell
        expand-region
        flycheck
        forge
        go-mode
        groovy-mode
        iedit
        ivy
        lsp-mode
        lsp-ui
        magit
        markdown-mode
        multiple-cursors
        org-bullets
        org-jira
        php-mode
        popup
        projectile
        protobuf-mode
        python-black
        pyvenv
        rg
        rust-mode
        quelpa
        quelpa-use-package
        scala-mode
        solarized-theme
        swiper
        undo-tree
        web-mode
        which-key
        yaml-mode
        ))
(defun install-packages ()
  "Install all selected packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))
(install-packages)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)
;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(load-theme 'solarized-light t)

;; Font setup
; (package-install 'fira-code-mode)
; (fira-code-mode-install-fonts)
; (global-fira-code-mode)

;; Configure undo tree mode -> C-x u
(global-undo-tree-mode t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))

;; Configure hide-show mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(setq hs-isearch-open t)
(global-set-key (kbd "C-'") 'hs-toggle-hiding)

;; Configure dired
(require 'dired)
(require 'dired-narrow)
(require 'dired-subtree)
(setq dired-listing-switches "-alh")
(setq dired-subtree-use-backgrounds nil)
(define-key dired-mode-map (kbd "b") 'dired-up-directory)
(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
(define-key dired-mode-map (kbd "I") 'dired-subtree-remove)
(define-key dired-mode-map (kbd "/") 'dired-narrow)

;; Configure flycheck
(require 'flycheck)
;; Use "pipx" to install flake8, pylint, and mypy.
(setq flycheck-python-flake8-executable "flake8")
(setq flycheck-python-pylint-executable "pylint")
(setq flycheck-python-mypy-executable "mypy")

(setq lsp-keymap-prefix "M-l")
(require 'lsp-mode)
(setq lsp-enable-snippet nil)
(setq lsp-prefer-flymake nil)
(setq lsp-pyls-configuration-sources ["flake8"])
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                  :major-modes '(python-mode)
                  :remote? t
                  :server-id 'pyls-remote))
(global-set-key (kbd "C-c i") 'lsp-mode)


; Configure iedit
(require 'iedit)

; Configure magit's forge
(setq auth-sources '("~/.authinfo"))
(require 'magit)
(require 'forge)
;; (push '("linkedin.githubprivate.com" "api.linkedin.githubprivate.com" "linkedin.githubprivate.com" forge-github-repository)
;;       forge-alist)

; Configure dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Old findstr.exe command.
;(defvar findstr-args-history nil)
;(defun findstr (dir cmd)
;  "Windows findstr.exe like rgrep."
;  (interactive
;   (list
;    (read-from-minibuffer "Directory: " default-directory)
;    (read-from-minibuffer "Arguments: " nil nil nil 'findstr-args-history)))
;  (setq old-default-directory default-directory)
;  (cd dir)
;  (compilation-start (format "findstr.exe %s" cmd) 'grep-mode)
;  (cd old-default-directory))

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files ".coverage")
     (add-to-list 'grep-find-ignored-directories ".tox")
     (add-to-list 'grep-find-ignored-directories ".mypy_cache")
     (add-to-list 'grep-find-ignored-directories ".pytest_cache")
     (add-to-list 'grep-find-ignored-directories "htmlcov")
     (add-to-list 'grep-find-ignored-directories "env27")
     (add-to-list 'grep-find-ignored-directories "env34")
     (add-to-list 'grep-find-ignored-directories "env35")
     (add-to-list 'grep-find-ignored-directories "env36")
     (add-to-list 'grep-find-ignored-directories "env37")
     (add-to-list 'grep-find-ignored-directories "env38")
     (add-to-list 'grep-find-ignored-directories "env39")
     (add-to-list 'grep-find-ignored-directories "env310")
     (add-to-list 'grep-find-ignored-directories "env311")
     (add-to-list 'grep-find-ignored-directories "env312")
     (add-to-list 'grep-find-ignored-directories "venv")
     (add-to-list 'grep-find-ignored-directories "env")))

(defun rgrepo (regexp &optional files dir confirm)
  "Wrap rgrep to temporarily add -o to grep-find-template."
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'rgrep))))
  (let ((original-grep-find-template grep-find-template)
        (new-grep-find-template
         (replace-regexp-in-string "<C>" "<C> -o" grep-find-template)))
    (setq grep-find-template new-grep-find-template)
    (apply #'rgrep regexp files dir confirm)
    (setq grep-find-template original-grep-find-template)))

(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)

(require 'multiple-cursors)
(global-unset-key "\C-xm")
(global-set-key (kbd "C-x m a") 'mc/mark-all-like-this)
; ^-- requires transient mark. Use C-space C-space to deactivate after C-x m a.
(global-set-key (kbd "C-x m s") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x m r") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x m i") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-x m l") 'mc/edit-lines)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/repos/"))
(setq projectile-sort-order 'recently-active)
(setq projectile-switch-project-action
      '(lambda ()
         (message "Changing to project: %s" default-directory)
         (projectile-dired)))
(setq projectile-project-root-files-functions
      '(projectile-root-local
        projectile-root-top-down
        projectile-root-top-down-recurring
        projectile-root-bottom-up))
(projectile-mode t)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "%3d %3d %3d" (count-lines start end) n (- end start))))

(defun uniq-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniq-buffer-lines ()
  "Remove duplicate adjacent lines in current buffer."
  (interactive)
  (unique-region-lines (point-min) (point-max)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-dabbrev-code-modes t)
(setq company-dabbrev-code-everywhere t)
(setq company-backends
      '(company-css
        (company-capf company-dabbrev-code company-keywords)
        company-files
        company-dabbrev))

(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (setq word-wrap t)
            (setq org-cycle-include-plain-lists t)
            (org-bullets-mode 1)))
(custom-theme-set-faces
 'solarized-light
 `(org-level-1 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-2 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-3 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-4 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-5 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-6 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-7 ((t (:weight bold :foreground "#268bd2"))))
 `(org-level-8 ((t (:weight bold :foreground "#268bd2")))))

;; (require 'org-jira)
;; (setq jiralib-url "https://jira01.corp.linkedin.com:8443")

(require 'ido)
(ido-mode t)
(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t)
(setq enable-recursive-minibuffers t)
(ido-everywhere)

(require 'ivy)
;; (ivy-mode 1)
(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key (kbd "C-c r") 'ivy-resume)
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)

; (require 'docker)
; (global-set-key (kbd "C-x d") 'docker)

(setq c-basic-offset 2)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; TODO: Setup C-c C-c to use %run command.
;; TODO: Setup ipython command to run IPython inside term.
;; (setq python-shell-interpreter
;;       "/Library/Frameworks/Python.framework/Versions/3.7/bin/ipython3")
;; (setq python-shell-interpreter-args
;;       "--colors=Linux --profile=default --simple-prompt")
;; (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
;; (setq python-shell-prompt-output-regexp "Out \\[[0-9]+\\]: ")
;; (setq python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion")
;; (setq python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n")
;; (setq python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'ansi-color)
(defun gj/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
(add-hook 'compilation-filter-hook
          #'gj/colorize-compilation)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-c t r") 'recompile)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq ein:output-area-inlined-images t)
(face-spec-set
 'ein:cell-input-area
 '((t nil))
 'face-defface-spec)

(autoload 'web-mode "web-mode"
  "Major mode for editing web files" t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'"))
)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)))

(add-to-list 'auto-mode-alist '("\\.css_t\\'" . css-mode))

;; (defun linkedin/mint-build ()
;;   (interactive)
;;   (compile "/bin/bash -ic \"mint build\""))
;; (global-set-key (kbd "<f5>") 'linkedin/mint-build)

;; (defun linkedin/git-review-create (testing bugs)
;;   (interactive "sTesting: \nsBugs: ")
;;   (compile
;;    (format "/bin/bash -ic \"git review create --no-prompt --owners --open --testing-done \\\"%s\\\" --bugs \\\"%s\\\"\"" testing bugs)))
;; (global-set-key (kbd "C-c l c") 'linkedin/git-review-create)

;; (defun linkedin/git-review-update ()
;;   (interactive)
;;   (compile
;;    (format "/bin/bash -ic \"git review update\"")))
;; (global-set-key (kbd "C-c l u") 'linkedin/git-review-update)

;; (defun linkedin/git-review-dcommit ()
;;   (interactive)
;;   (compile
;;    (format "/bin/bash -ic \"git review dcommit\"")))
;; (global-set-key (kbd "C-c l d") 'linkedin/git-review-dcommit)

;; (defun linkedin/git-push ()
;;   (interactive)
;;   (compile
;;    (format "/bin/bash -ic \"git push origin HEAD:master\"")))
;; (global-set-key (kbd "C-c l p") 'linkedin/git-push)

(defun git-push-dokku ()
  (interactive)
  (compile
   (format "/bin/bash -ic \"git push dokku main:main\"")))

(defun blue ()
  (interactive)
  (compile
   (format "/bin/bash -ic \"blue %s\"" (buffer-file-name)))
  (revert-buffer (buffer-file-name)))

;; $ brew install w3m
;; M-x ddg Search: bill gates Enter
;; Supports Duck Duck Go's "bang" syntax. For example:
;; !dict duck typing    <-- Dictionary lookup
;; !pep 8               <-- Python PEPs
;; !ducky python numpy  <-- I'm feeling lucky
(defun ddg (search-query)
  (interactive
   (list
    (read-from-minibuffer "Search: ")))
  (shell-command
   (format "/bin/bash -ic \"w3m 'https://duckduckgo.com/lite?kd=-1&q=%s'\"" search-query)))

;; M-x zone
(setq zone-programs [zone-pgm-drip])

;; Configure ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)
(setq aw-dispatch-always t)

(require 'use-package)
(require 'quelpa-use-package)
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

(require 'rg)
(rg-enable-default-bindings)

(defun open-github-pr ()
  "Open a new GitHub pull request in the default browser using the current branch."
  (interactive)
  (let* ((current-branch (magit-get-current-branch))
         (remote-url (magit-get "remote" "origin" "url"))
         (github-url (if (string-match "github\\.com[:/]\\(.*?\\)\\.git$" remote-url)
                         (match-string 1 remote-url)
                       (error "Could not parse GitHub URL")))
         (pr-url (format "https://github.com/%s/compare/%s?expand=1" github-url current-branch)))
    (browse-url pr-url)))
(global-set-key (kbd "C-c m p") 'open-github-pr)

(require 'which-key)
(which-key-mode)

(require 'realgud)
(global-set-key (kbd "C-c d p") 'realgud:pdb-remote)
(global-set-key (kbd "C-c d q") 'realgud:cmd-quit)
(global-set-key (kbd "C-c d d") 'realgud-short-key-mode)

(defun send-buffer-to-llm (use-prompt)
  "Write the buffer to a temporary file, then invoke `llm` in a uniquely named compilation buffer with line wrapping.
If called with a prefix argument, ask the user for a prompt and include it in the `llm` command."
  (interactive "P")  ; "P" for prefix argument
  (let* ((temp-file (make-temp-file "emacs-llm-"))
         (output-buffer (generate-new-buffer-name "*LLM Output*"))
         (prompt (if use-prompt
                     (read-string "Prompt: ")
                   ""))
         (command (if use-prompt
                      (format "cat %s | llm \"%s\"" temp-file (shell-quote-argument prompt))
                    (format "cat %s | llm" temp-file))))
    (write-region (point-min) (point-max) temp-file)
    (compilation-start command
                       'compilation-mode
                       (lambda (_) output-buffer))
    (with-current-buffer output-buffer
      (setq truncate-lines nil))))

(global-set-key (kbd "C-c l b") 'send-buffer-to-llm)

(defvar gj-llm-state-stack nil
  "Stack of LLM states, each containing b1, r1, a1, and original-buffer.")

(defun send-region-to-llm (start end use-prompt)
  "Send the selected region to `llm` and store surrounding text on a stack for later diff.
If called with a prefix argument, ask the user for a prompt and include it in the `llm` command."
  (interactive "r\nP")
  ;; Create a state plist and push onto the stack
  (push (list :b1 (buffer-substring-no-properties (point-min) start)
              :r1 (buffer-substring-no-properties start end)
              :a1 (buffer-substring-no-properties end (point-max))
              :original-buffer (current-buffer))
        gj-llm-state-stack)
  ;; Prepare the command and output buffer
  (let* ((temp-file (make-temp-file "emacs-llm-"))
         (prompt (if use-prompt
                     (read-string "Prompt: ")
                   ""))
         (command (if use-prompt
                      (format "cat %s | llm %s"
                              (shell-quote-argument temp-file)
                              (shell-quote-argument prompt))
                    (format "cat %s | llm"
                            (shell-quote-argument temp-file))))
         (output-buffer-name (generate-new-buffer-name "*LLM Output*")))
    ;; Write the selected region to the temporary file
    (write-region start end temp-file)
    ;; Start the compilation process
    (compilation-start command
                       'compilation-mode
                       (lambda (_) output-buffer-name))
    ;; Display the output buffer
    (let ((output-buffer (get-buffer output-buffer-name)))
      (when output-buffer
        (with-current-buffer output-buffer
          (setq truncate-lines nil)
          ;; Optional: Set read-only mode off if you want to edit the output buffer
          (read-only-mode -1))
        (display-buffer output-buffer)))))

(global-set-key (kbd "C-c l r") 'send-region-to-llm)

(defun ediff-llm-region-with-original (start end)
  "Replace the original region with the selected LLM output and diff with the original buffer.
Select the new region (`r2`) in the LLM output buffer or any buffer, then invoke this function."
  (interactive "r")
  (unless gj-llm-state-stack
    (error "No stored LLM state. Please run `send-region-to-llm` first."))
  ;; Get the top state from the stack
  (let* ((state (car gj-llm-state-stack))
         (b1 (plist-get state :b1))
         (r1 (plist-get state :r1))
         (a1 (plist-get state :a1))
         (original-buffer (plist-get state :original-buffer))
         ;; Get `r2` from the selected region
         (r2 (buffer-substring-no-properties start end))
         (modified-buffer-name (concat (buffer-name original-buffer) "-llm-modified"))
         (modified-buffer (get-buffer-create modified-buffer-name)))
    ;; Create the modified buffer with `b1 + r2 + a1`
    (with-current-buffer modified-buffer
      (erase-buffer)
      (insert b1)
      (insert r2)
      (insert a1))
    ;; Run ediff between the original buffer and the modified buffer
    (ediff-buffers original-buffer modified-buffer)))

(global-set-key (kbd "C-c l e") 'ediff-llm-region-with-original)

(defun push-llm-state (start end)
  "Push the current region onto the LLM state stack.
Stores the text before the region (`b1`), the region itself (`r1`), the text after the region (`a1`), and the original buffer."
  (interactive "r")
  ;; Create a state plist and push onto the stack
  (push (list :b1 (buffer-substring-no-properties (point-min) start)
              :r1 (buffer-substring-no-properties start end)
              :a1 (buffer-substring-no-properties end (point-max))
              :original-buffer (current-buffer))
        gj-llm-state-stack)
  (message "Pushed LLM state onto the stack"))

(global-set-key (kbd "C-c l o") 'pop-llm-state)

(defun pop-llm-state ()
  "Pop the last stored LLM state from the stack."
  (interactive)
  (if gj-llm-state-stack
      (progn
        (setq gj-llm-state-stack (cdr gj-llm-state-stack))
        (message "Popped LLM state from the stack"))
    (message "No LLM state to pop")))

(global-set-key (kbd "C-c l p") 'pop-llm-state)

(defun send-windows-to-llm (use-prompt)
  "Send the contents of all displayed buffers to `llm`."
  (interactive "P")  ; "P" for prefix argument
  (let* ((buffers (delete-dups (mapcar 'window-buffer (window-list))))
         (temp-file (make-temp-file "emacs-llm-"))
         (output-buffer (generate-new-buffer-name "*LLM Output*"))
         (prompt (if use-prompt
                     (read-string "Prompt: ")
                   ""))
         (command (if use-prompt
                      (format "cat %s | llm \"%s\"" temp-file (shell-quote-argument prompt))
                    (format "cat %s | llm" temp-file))))
    (with-temp-file temp-file
      (dolist (buffer buffers)
        (insert (format "<buffer name: %s>\n" (buffer-name buffer)))
        (insert "----\n")
        (insert-buffer-substring buffer)
        (insert "\n----\n")))

    ;; Start the compilation process
    (compilation-start command
                       'compilation-mode
                       (lambda (_) output-buffer))
    (with-current-buffer output-buffer
      (setq truncate-lines nil))))

(global-set-key (kbd "C-c l w") 'send-windows-to-llm)

(defun send-prompt-to-llm (use-c-flag)
  "Ask the user for a prompt, then invoke `llm` in a uniquely named compilation buffer with line wrapping.
If called with a prefix argument, add the `-c` flag to the `llm` command."
  (interactive "P")  ; P means provide the prefix argument as an argument to the function
  (let ((prompt (read-string "Prompt: "))
        (temp-file (make-temp-file "emacs-llm-"))
        (output-buffer (generate-new-buffer-name "*LLM Output*"))
        (llm-command "llm"))
    (with-temp-file temp-file
      (insert prompt))
    (when use-c-flag
      (setq llm-command (concat llm-command " -c")))
    (let ((command (format "cat %s | %s" temp-file llm-command)))
      (compilation-start command
                         'compilation-mode
                         (lambda (_) output-buffer)))
    (with-current-buffer output-buffer
      (setq truncate-lines nil))))

(global-set-key (kbd "C-c l p") 'send-prompt-to-llm)

(defun run-llm-chat (use-c-flag)
  "Run `llm chat` in comint mode.
With prefix argument, run `llm chat -c` instead."
  (interactive "P")
  (let ((buffer-name "*LLM Chat*")
        (command "llm chat"))
    (when use-c-flag
      (setq command (concat command " -c")))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq truncate-lines nil)
      (comint-mode)
      (comint-exec buffer-name buffer-name shell-file-name nil (list shell-command-switch command))
      (pop-to-buffer buffer-name))))

(global-set-key (kbd "C-c l c") 'run-llm-chat)

(defun git-update-branch ()
  "Merge master and push without verification from the root git directory."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (when default-directory
      (let ((output-buffer (generate-new-buffer-name "*Git Merge and Push*")))
        (compilation-start "(git merge master -m \"Merge branch 'master'\" && git push --no-verify) || git status"
                           'compilation-mode
                           (lambda (_) output-buffer))))))

(defun git-update-branch ()
  "Update the current branch by merging master and pushing without verification."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (when default-directory
      (let ((current-branch (magit-get-current-branch))
            (output-buffer (generate-new-buffer-name "*Git Update Branch*")))
        (compilation-start (format "(git checkout master \
                                     && git pull \
                                     && git checkout %s \
                                     && git merge master -m \"Merge branch 'master'\" \
                                     && git push --no-verify) \
                                    || git status"
                                   current-branch)
                           'compilation-mode
                           (lambda (_) output-buffer))))))
