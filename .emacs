;; .emacs file
;; Copyright 2015 Grant Jenks

(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'custom-theme-load-path (concat (file-name-directory load-file-name) "/themes"))

(load-theme 'solarized t)

(setq frame-title-format "%b")
(setq gc-cons-threshold 20000000)
(setq inhibit-startup-screen t)
(setq mac-command-modifier 'control)
(setq visible-bell t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(setq initial-scratch-message ";;# M-: (eval-expression) C-x C-e (eval-last-sexp) [C-u] C-M-x [debug] (eval-defun)

")

(delete-selection-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(recentf-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)

(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-M-g") 'find-file-at-point)

;; Store backups in one place.
(setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Hide window bars.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use utf-8 everywhere.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Smooth scrolling.
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Move buffers around.
(require 'buffer-move)
(global-set-key (kbd "C-M-^") 'buf-move-left)
(global-set-key (kbd "C-M-(") 'buf-move-right)
(global-set-key (kbd "C-M-&") 'buf-move-up)
(global-set-key (kbd "C-M-*") 'buf-move-down)

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

(defvar findstr-args-history nil)
(defun findstr (dir cmd)
  "Windows findstr.exe like rgrep."
  (interactive
   (list
    (read-from-minibuffer "Directory: " default-directory)
    (read-from-minibuffer "Arguments: " nil nil nil 'findstr-args-history)))
  (setq old-default-directory default-directory)
  (cd dir)
  (compilation-start (format "findstr.exe %s" cmd) 'grep-mode)
  (cd old-default-directory))

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

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (setq word-wrap t)
            (setq org-cycle-include-plain-lists t)))

(require 'ido)
(ido-mode t)
(setq ido-use-virtual-buffers t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'web-mode "web-mode"
  "Major mode for editing web files" t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'"))
)
(defun gmj-web-mode-hook ()
  "Hooks for web-mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'gmj-web-mode-hook)

(require 'hippie-exp)
(setq hippie-expand-try-functions-list
              (quote (try-expand-all-abbrevs
                      try-expand-line
                      try-expand-dabbrev
                      try-expand-dabbrev-visible)))

(defvar gmj-keys-minor-mode-map (make-keymap) "gmj-keys-minor-mode keymap.")

(define-key gmj-keys-minor-mode-map (kbd "M-SPC") 'hippie-expand)
(define-key gmj-keys-minor-mode-map (kbd "C-M-6") 'windmove-left)
(define-key gmj-keys-minor-mode-map (kbd "C-M-9") 'windmove-right)
(define-key gmj-keys-minor-mode-map (kbd "C-M-7") 'windmove-up)
(define-key gmj-keys-minor-mode-map (kbd "C-M-8") 'windmove-down)

(define-minor-mode gmj-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " gmj-keys" 'gmj-keys-minor-mode-map)

(gmj-keys-minor-mode 1)

;; Disable my key settings in the minibuffer.
(defun gmj-minibuffer-setup-hook ()
  (gmj-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'gmj-minibuffer-setup-hook)

;; In order for the above to work in more cases (like flyspell):
(defadvice load (after give-gmj-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'gmj-keys-minor-mode))
      (let ((gmjkeys (assq 'gmj-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'gmj-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist gmjkeys))))
(ad-activate 'load)
