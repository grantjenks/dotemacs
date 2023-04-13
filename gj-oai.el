(require 'request)

(defun read-api-key-from-dotenv (file)
  "Read the OpenAI API key from the dotenv FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^export OPENAI_API_KEY=\\(.*\\)$" nil t)
      (match-string 1))))

(defun chat-completions-gpt-4 (message)
  "Query the OpenAI Chat Completions endpoint using gpt-4 model with MESSAGE."
  (interactive "sEnter your message: ")
  (let* ((dotenv-file (expand-file-name ".env" "~"))
         (api-key (read-api-key-from-dotenv dotenv-file))
         (url "https://api.openai.com/v1/chat/completions")
         (data (json-encode-alist `(("model" . "gpt-4")
                                     ("messages" . [(( "role" . "user") ("content" . ,message))])))))
    (if api-key
        (request
         url
         :type "POST"
         :headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " api-key)))
         :data data
         :parser 'json-read
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((response (aref (alist-get 'choices data) 0)))
                       (message "Response: %s" (alist-get 'content (alist-get 'message response)))))))
      (message "Error: API key not found in %s" dotenv-file))))

(global-set-key (kbd "C-c g") 'chat-completions-gpt-4)
