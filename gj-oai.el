(defun gj-chat-completion ()
  "Send a query to a Chat Completion process and display the output in a new buffer."
  (interactive)
  (let ((query "")
        (oai-script-path (expand-file-name "~/repos/dotemacs/oai")))
    (if (region-active-p)
        (progn
          (let ((prefix (read-string "Enter prefix: "))
                (suffix (read-string "Enter suffix: "))
                (region-content (buffer-substring-no-properties (region-beginning) (region-end))))
            (setq query (format "%s\n```\n%s\n```\n%s" prefix region-content suffix))
            (deactivate-mark)))
      (setq query (read-string "Enter query: ")))
    (let ((temp-file (make-temp-file "chat-completion-")))
      (with-temp-file temp-file
        (insert query))
      (compile (format "/bin/bash -ic \"cat '%s' | %s && rm '%s'\""
                       (shell-quote-argument temp-file)
                       oai-script-path
                       (shell-quote-argument temp-file))
               t)
      (setq-local compilation-read-command nil)
      (pop-to-buffer compilation-last-buffer))))
