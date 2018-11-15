;; # Emacs Config File: .emacs
;; Copyright 2015-2018 Grant Jenks
;;
;; TODO
;; https://stackoverflow.com/questions/12058717/confusing-about-the-emacs-custom-system
;;
;; ## Reminders
;;
;; C-SPC C-SPC set mark and disable region
;; C-u C-SPC jump to the mark
;; M-a / M-b forward / back sentence
;; C-M-a C-M-e begging/end of defun
;; M-r cycle positions in window (middle-top-bottom)
;; C-x r (m|b|l) registers/bookmarks
;; C-x r s [name] save region to register
;; C-x r i [name] insert region from register
;;
;; ### Shell commands
;;
;; M-! execute shell command
;; M-& execute shell command async
;; M-| send region to shell command
;; With C-u, output command to buffer
;;
;; ### Elisp Commands
;;
;; M-: eval-expression
;; C-x C-e eval-last-sexp
;; [C-u] C-M-x [debug] eval-defun
;;

(setq dotemacs-directory (file-name-directory load-file-name))
(setq themes-directory (concat dotemacs-directory "themes"))
(add-to-list 'load-path dotemacs-directory)

(add-to-list 'custom-theme-load-path themes-directory)
(cond ((display-graphic-p) (load-theme 'solarized t))
      (t (load-theme 'whiteboard)))

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
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

(delete-selection-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(recentf-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'rb 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'bw 'balance-windows)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)

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

;; Setup MELPA.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; '(custom-safe-themes
 ;;   (quote
 ;;    ("31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" default)))
 '(package-selected-packages (quote (company-tabnine popup ctable concurrent company))))

;; Move buffers around.
(require 'buffer-move)
(global-set-key (kbd "C-M-^") 'buf-move-left)
(global-set-key (kbd "C-M-(") 'buf-move-right)
(global-set-key (kbd "C-M-&") 'buf-move-up)
(global-set-key (kbd "C-M-*") 'buf-move-down)

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files ".coverage")
     (add-to-list 'grep-find-ignored-directories ".tox")
     (add-to-list 'grep-find-ignored-directories "env27")
     (add-to-list 'grep-find-ignored-directories "env34")
     (add-to-list 'grep-find-ignored-directories "env35")
     (add-to-list 'grep-find-ignored-directories "env36")
     (add-to-list 'grep-find-ignored-directories "env37")
     (add-to-list 'grep-find-ignored-directories "env")))

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

(defun unique-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun unique-buffer-lines ()
  "Remove duplicate adjacent lines in current buffer."
  (interactive)
  (unique-region-lines (point-min) (point-max)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-dabbrev-code-modes t)
(setq company-dabbrev-code-everywhere t)
(setq company-backends
      '(company-css
        (company-dabbrev-code company-keywords)
        company-files
        company-dabbrev))
(require 'company-tabnine)
(add-to-list 'company-backends 'company-tabnine)

(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (setq word-wrap t)
            (setq org-cycle-include-plain-lists t)))

(require 'ido)
(ido-mode t)
(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t)

(setq c-basic-offset 2)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

(defvar gmj-keys-minor-mode-map (make-keymap) "gmj-keys-minor-mode keymap")

(define-key gmj-keys-minor-mode-map (kbd "C-M-6") 'windmove-left)
(define-key gmj-keys-minor-mode-map (kbd "C-M-9") 'windmove-right)
(define-key gmj-keys-minor-mode-map (kbd "C-M-7") 'windmove-up)
(define-key gmj-keys-minor-mode-map (kbd "C-M-8") 'windmove-down)

(define-minor-mode gmj-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " gmj-keys" 'gmj-keys-minor-mode-map)

(gmj-keys-minor-mode 1)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (gmj-keys-minor-mode 0)))

;; In order for the above to work in more cases (like flyspell):
(defadvice load (after give-gmj-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'gmj-keys-minor-mode))
      (let ((gmjkeys (assq 'gmj-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'gmj-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist gmjkeys))))
(ad-activate 'load)
