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
  (compile (format "/bin/bash -ic \"echo '%s' | %s %s %s\""
                   (if (region-active-p)
                       (concat prefix "\n```\n" query "\n```\n" suffix)
                     query)
                   "/Users/grantjenks/repos/dotemacs/oai"
                   "-"
                   "-")
           t)
  (setq-local compilation-read-command nil)
  (pop-to-buffer compilation-last-buffer))
