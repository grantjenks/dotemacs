;;; tab-line-move.el --- Tab-line move/solo helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar gj/tab-line--orig-tabs-function nil
  "Saved original value of `tab-line-tabs-function'.")

(defun gj/tab-line--base-buffers ()
  "Return the buffers tab-line would normally show for the current window."
  (let ((f (or gj/tab-line--orig-tabs-function
               (and (not (eq tab-line-tabs-function #'gj/tab-line-tabs-reordered))
                    tab-line-tabs-function)
               #'tab-line-tabs-window-buffers)))
    (let ((tabs (funcall f)))
      (cl-loop for x in tabs
               if (bufferp x) collect x
               else if (and (stringp x) (get-text-property 0 'buffer x))
               collect (get-text-property 0 'buffer x)
               else if (and (consp x) (bufferp (cdr (assq 'buffer x))))
               collect (cdr (assq 'buffer x))))))

(defun gj/tab-line--ensure ()
  "Ensure our reordering function is installed."
  (unless (eq tab-line-tabs-function #'gj/tab-line-tabs-reordered)
    (setq gj/tab-line--orig-tabs-function tab-line-tabs-function)
    (setq tab-line-tabs-function #'gj/tab-line-tabs-reordered)))

(defun gj/tab-line--sync-order (win base)
  "Sync WIN's stored order with BASE buffer list. Return a *copy* of the order."
  (let* ((old (copy-sequence (or (window-parameter win 'gj/tab-order) '())))
         (order (cl-remove-if-not (lambda (b) (and (buffer-live-p b) (memq b base)))
                                  old)))
    (dolist (b base)
      (unless (memq b order)
        (setq order (append order (list b)))))
    (setq order (copy-sequence order))
    (set-window-parameter win 'gj/tab-order order)
    (copy-sequence order)))

(defun gj/tab-line-tabs-reordered ()
  "Tabs function honoring per-window order (returns a fresh copy)."
  (let* ((win (selected-window))
         (base (gj/tab-line--base-buffers)))
    (copy-sequence (gj/tab-line--sync-order win base))))

(defun gj/tab-line--swap (order i j)
  "Swap elements at I and J in list ORDER, returning a *new list*."
  (let ((new (copy-sequence order)))
    (let ((bi (nth i new))
          (bj (nth j new)))
      (when (and bi bj)
        (setf (nth i new) bj
              (nth j new) bi)))
    new))

(defun gj/tab-line--refresh (_win)
  "Refresh tab-line, busting caches and redisplaying."
  (when (boundp 'tab-line--format-cache)
    (setq tab-line--format-cache nil))
  (force-mode-line-update t)
  (redisplay t))

;;;###autoload
(defun gj/tab-line-move-current-tab-left ()
  "Move the current tab one position to the left."
  (interactive)
  (gj/tab-line--ensure)
  (let* ((win (selected-window))
         (base (gj/tab-line--base-buffers))
         (order (gj/tab-line--sync-order win base))
         (buf (window-buffer win))
         (idx (cl-position buf order :test #'eq)))
    (when (and idx (> idx 0))
      (set-window-parameter win 'gj/tab-order
                            (gj/tab-line--swap order idx (1- idx)))
      (gj/tab-line--refresh win))))

;;;###autoload
(defun gj/tab-line-move-current-tab-right ()
  "Move the current tab one position to the right."
  (interactive)
  (gj/tab-line--ensure)
  (let* ((win (selected-window))
         (base (gj/tab-line--base-buffers))
         (order (gj/tab-line--sync-order win base))
         (buf (window-buffer win))
         (idx (cl-position buf order :test #'eq)))
    (when (and idx (< idx (1- (length order))))
      (set-window-parameter win 'gj/tab-order
                            (gj/tab-line--swap order idx (1+ idx)))
      (gj/tab-line--refresh win))))

;;;###autoload
(defun gj/tab-line-bury-other-tabs ()
  "Bury all other buffers shown as tabs in the current window.
Keeps only the current buffer visible in the tab line. Does not kill buffers."
  (interactive)
  (gj/tab-line--ensure)
  (let* ((win (selected-window))
         (cur (window-buffer win))
         (tabs (gj/tab-line--base-buffers)))
    ;; Bury everything but the current buffer.
    (dolist (b tabs)
      (when (and (buffer-live-p b) (not (eq b cur)))
        (bury-buffer b)))
    ;; Reset per-window order/history so only CUR remains.
    (set-window-parameter win 'gj/tab-order (list cur))
    (when (fboundp 'set-window-prev-buffers) (set-window-prev-buffers win nil))
    (when (fboundp 'set-window-next-buffers) (set-window-next-buffers win nil))
    (gj/tab-line--refresh win)))

;; Keybindings for tab moves (active when tab-line is loaded)
(with-eval-after-load 'tab-line
  (define-key tab-line-mode-map (kbd "C-M-{") #'gj/tab-line-move-current-tab-left)
  (define-key tab-line-mode-map (kbd "C-M-}") #'gj/tab-line-move-current-tab-right))

;; Global key for “solo this tab” (bury others)
(global-set-key (kbd "C-x t 1") #'gj/tab-line-bury-other-tabs)

(provide 'tab-line-move)
;;; tab-line-move.el ends here
