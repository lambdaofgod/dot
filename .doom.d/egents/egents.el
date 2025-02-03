(defvar egents/args (concat "python :session egents :async" default-code-block-args))
(defvar egents/python-code-dir (file-name-directory load-file-name))
(defvar egents/python-path (file-name-directory "~/.pyenv/shims/python"))

(defun read-file-as-string (filename)
    (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string)))



(defun egents/init-python-code (key-path)
    (format (read-file-as-string (concat egents/python-code-dir "agent_setup.py")) key-path))


(defun egents/initialize-agent ()
    (interactive)
    (let ((key-path (read-string "key path, default: " "~/.keys/anthropic_key.txt")))
        (insert-org-mode-block-with-content egents/args (egents/init-python-code key-path))))


(defun egents/insert-response-code-block (chatgpt-query)
    (interactive "sAsk agent: ")
    (insert-org-mode-block-with-content egents/args
        (format "prompt = '%s'\nget_agent_output(prompt)" chatgpt-query)))
