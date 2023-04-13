;; TODO
;; - Region behavior does not work!

(defun chat-completion (prefix suffix query)
  "Send a query to a Chat Completion process and display the output in a new buffer."
  (interactive
   (if (region-active-p)
       (list
        (read-string "Prefix: ")
        (read-string "Suffix: ")
        (buffer-substring-no-properties (region-beginning) (region-end)))
     (list
      nil
      nil
      (read-string "Query: "))))
  (let ((temp-file (make-temp-file "chat-completion-")))
    (with-temp-file temp-file
      (insert (if (region-active-p)
                  (concat prefix "\n```\n" query "\n```\n" suffix)
                query)))
    (compile (format "/bin/bash -ic \"cat '%s' | /Users/grantjenks/repos/dotemacs/oai && rm '%s'\""
                     (shell-quote-argument temp-file)
                     (shell-quote-argument temp-file))
             t)
    (setq-local compilation-read-command nil)
    (pop-to-buffer compilation-last-buffer)))
